from datetime import datetime, timedelta
from geopy import Nominatim
from timezonefinder import TimezoneFinder
from pytz import timezone, utc
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle
from skyfield.api import Star, load, wgs84
from skyfield.data import hipparcos
from skyfield.projections import build_stereographic_projection
import imageio.v2 as imageio
import os

print("=" * 58)
print("CREANDO ANIMACIÓN DEL CIELO NOCTURNO PARA ARTIEDA EN MARZO")
print("=" * 58)

# ============================================================================
# CONFIGURACIÓN
# ============================================================================
LOCATION_NAME = 'Artieda, Zaragoza, Spain'
START_DATE = '2026-03-05'
START_HOUR = 19  # 7 PM
END_HOUR = 7     # 7 AM (día siguiente)
INTERVAL_MINUTES = 5  # Fotograma cada 10 minutos
LIMITING_MAGNITUDE = 9  # Estrellas visibles (menor = menos estrellas pero más claras)
CHART_SIZE = 12
MAX_STAR_SIZE = 100
OUTPUT_FILENAME = 'cielo_nocturno_artieda.gif'
FPS = 20  # Frames por segundo (duration = 1/FPS) 

## Creo que los FPS no estan funcionando del todo, puede que haya un límite de FPS para los gifs.

# ============================================================================
# CARGAR DATOS
# ============================================================================
print("\nCargando datos...")
eph = load('de421.bsp')
with load.open(hipparcos.URL) as f:
    stars = hipparcos.load_dataframe(f)
print(f"Estrellas: {len(stars)}")

# Ubicación
locator = Nominatim(user_agent='myGeocoder')
location = locator.geocode(LOCATION_NAME)
lat, long = location.latitude, location.longitude

# Zona horaria
tf = TimezoneFinder()
timezone_str = tf.timezone_at(lat=lat, lng=long)
local_tz = timezone(timezone_str)

print(f"Ubicación: {lat:.4f}°N, {long:.4f}°E")
print(f"Zona horaria: {timezone_str}")

# ============================================================================
# CALCULAR FRAMES
# ============================================================================
total_hours = 12
num_frames = int((total_hours * 60) / INTERVAL_MINUTES)
print(f"\nGenerando {num_frames} fotogramas ({INTERVAL_MINUTES} min cada uno)")

sun = eph['sun']
earth = eph['earth']
ts = load.timescale()

frames = []
temp_dir = 'temp_frames_mejorado'
os.makedirs(temp_dir, exist_ok=True)

# ============================================================================
# GENERAR FOTOGRAMAS
# ============================================================================
for i in range(num_frames):
    # Calcular tiempo
    hours_elapsed = (INTERVAL_MINUTES * i) / 60.0
    current_hour = START_HOUR + hours_elapsed
    
    if current_hour >= 24:
        current_date = datetime.strptime(START_DATE, '%Y-%m-%d') + timedelta(days=1)
        current_hour = current_hour - 24
    else:
        current_date = datetime.strptime(START_DATE, '%Y-%m-%d')
    
    hour = int(current_hour)
    minute = int((current_hour - hour) * 60)
    dt = current_date.replace(hour=hour, minute=minute)
    
    # UTC
    local_dt = local_tz.localize(dt, is_dst=None)
    utc_dt = local_dt.astimezone(utc)
    
    # Observador
    t = ts.from_datetime(utc_dt)
    observer = wgs84.latlon(latitude_degrees=lat, longitude_degrees=long).at(t)
    
    # Proyección
    ra, dec, distance = observer.radec()
    center_object = Star(ra=ra, dec=dec)
    center = earth.at(t).observe(center_object)
    projection = build_stereographic_projection(center)
    
    # Posiciones
    star_positions = earth.at(t).observe(Star.from_dataframe(stars))
    stars['x'], stars['y'] = projection(star_positions)
    
    # Filtrar
    bright_stars = (stars.magnitude <= LIMITING_MAGNITUDE)
    magnitude = stars['magnitude'][bright_stars]
    
    # ========================================================================
    # CREAR GRÁFICO
    # ========================================================================
    fig, ax = plt.subplots(figsize=(CHART_SIZE, CHART_SIZE), facecolor='#000814')
    ax.set_facecolor('#000814')
    
    # Fondo degradado (simulando atmósfera)
    border = Circle((0, 0), 1, color='#001d3d', fill=True, zorder=0)
    ax.add_patch(border)
    
    # Círculos de altitud (30°, 60°, horizonte)
    for radius, alpha in [(0.33, 0.15), (0.66, 0.12), (1.0, 0.3)]:
        circle = Circle((0, 0), radius, fill=False, color='#ffc300',
                       alpha=alpha, linewidth=1.2, linestyle='--', zorder=1)
        ax.add_patch(circle)
        
        # Etiquetas de altitud
        if radius < 1.0:
            alt_degrees = int((1 - radius) * 90)
            ax.text(0, radius + 0.03, f'{alt_degrees}°', 
                   color='#ffc300', alpha=0.4, ha='center', fontsize=9)
    
    # Direcciones cardinales
    directions = {
        'N': (0, 1.05), 
        'S': (0, -1.05), 
        'E': (1.05, 0), 
        'O': (-1.05, 0)
    }
    for label, (x, y) in directions.items():
        ax.text(x, y, label, color="#36352D", alpha=0.7,
               ha='center', va='center', fontsize=14, fontweight='bold')
    
    # Estrellas
    marker_size = 2 * MAX_STAR_SIZE * 10 ** (magnitude / -2.5)
    
    # Añadir brillo a las estrellas más brillantes
    for mag_threshold, color, alpha_val in [
        (1.5, '#ffffff', 1.0),    # Muy brillantes: blanco puro
        (3.0, '#ffffcc', 0.95),   # Brillantes: blanco cálido
        (6.0, '#ccccff', 0.85),   # Normales: blanco azulado
    ]:
        mask = bright_stars & (stars['magnitude'] <= mag_threshold)
        if mask.sum() > 0:
            sizes = 2 * MAX_STAR_SIZE * 10 ** (stars['magnitude'][mask] / -2.5)
            ax.scatter(stars['x'][mask], stars['y'][mask],
                      s=sizes, color=color, marker='.', 
                      linewidths=0, alpha=alpha_val, zorder=3)
    
    # Resto de estrellas
    normal_stars = bright_stars & (stars['magnitude'] > 3.0)
    if normal_stars.sum() > 0:
        sizes = 2 * MAX_STAR_SIZE * 10 ** (stars['magnitude'][normal_stars] / -2.5)
        ax.scatter(stars['x'][normal_stars], stars['y'][normal_stars],
                  s=sizes, color='white', marker='.', 
                  linewidths=0, alpha=0.8, zorder=2)
    
    # Recortar
    horizon = Circle((0, 0), radius=1, transform=ax.transData)
    for col in ax.collections:
        col.set_clip_path(horizon)
    
    # Configuración
    ax.set_xlim(-1.15, 1.15)
    ax.set_ylim(-1.15, 1.15)
    ax.axis('off')
    
    # Título dinámico
    time_str = dt.strftime('%H:%M')
    date_str = dt.strftime('%d de marzo de %Y')
    
    # Determinar si es noche o madrugada
    if hour >= 19 or hour < 7:
        periodo = "Noche" if hour >= 19 else "Madrugada"
    else:
        periodo = "Día"
    
    title_text = f'Mapa del cielo nocturno en Artieda\n{date_str} · {time_str} ({periodo})'
    plt.title(title_text, color='#ffd60a', fontsize=18, pad=25, fontweight='bold')
    
    # Guardar
    frame_path = f'{temp_dir}/frame_{i:03d}.png'
    plt.savefig(frame_path, dpi=120, bbox_inches='tight', 
                facecolor='#000814', edgecolor='none')
    frames.append(frame_path)
    plt.close()
    
    # Progreso
    progress = int((i + 1) / num_frames * 100)
    if (i + 1) % 3 == 0 or i == 0 or i == num_frames - 1:
        print(f"  [{progress:3d}%] Frame {i+1}/{num_frames} - {time_str}")

# ============================================================================
# CREAR GIF
# ============================================================================
print("\n\nCreando GIF animado...")

images = []
for frame_path in frames:
    images.append(imageio.imread(frame_path))

duration = 1.0 / FPS  # Duración por frame en segundos
imageio.mimsave(OUTPUT_FILENAME, images, duration=duration, loop=0)

file_size_mb = os.path.getsize(OUTPUT_FILENAME) / (1024 * 1024)

print(f"\n{'='*70}")
print(f"¡GIF CREADO EXITOSAMENTE!")
print(f"{'='*70}")
print(f"  Archivo:    {OUTPUT_FILENAME}")
print(f"  Tamaño:     {file_size_mb:.2f} MB")
print(f"  Fotogramas: {len(images)}")
print(f"  FPS:        {FPS}")
print(f"  Duración:   {len(images) * duration:.1f} segundos")
print(f"{'='*70}")

# Limpiar
print("\nLimpiando archivos temporales...")
for frame_path in frames:
    os.remove(frame_path)
os.rmdir(temp_dir)

print("Limpieza completada\n")
