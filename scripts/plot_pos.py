import pandas as pd
from datetime import datetime
import plotly.graph_objects as go
from statsmodels.nonparametric.smoothers_lowess import lowess
import numpy as np
import argparse

def parse_pos_format(file_path):
    data = []
    try:
        with open(file_path, 'r') as file:
            data_started = False
            for line in file:
                if line.startswith('*'):
                    data_started = True
                    continue
                if data_started:
                    parts = line.split()
                    if len(parts) >= 24:
                        record = {
                            'Time': datetime.strptime(parts[0], '%Y-%m-%dT%H:%M:%S.%f'),
                            'Latitude': float(parts[11]),
                            'Longitude': float(parts[12]),
                            'Elevation': float(parts[13]),
                            'dN': float(parts[14]),
                            'dE': float(parts[15]),
                            'dU': float(parts[16]),
                            'sN': float(parts[17]),
                            'sE': float(parts[18]),
                            'sU': float(parts[19]),
                            'sElevation': float(parts[19]),
                            'Rne': float(parts[20]),
                            'Rnu': float(parts[21]),
                            'Reu': float(parts[22]),
                            'soln': parts[23]
                        }
                        data.append(record)
    except Exception as e:
        print(f"Error parsing file {file_path}: {e}")                
    return pd.DataFrame(data)

# Function to parse the datetime with optional timezone
def parse_datetime(datetime_str):
    # Attempt to parse datetime with and without timezone
    for fmt in ("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%S"):
        try:
            parsed_datetime = datetime.strptime(datetime_str, fmt)
            # Make datetime timezone-naive if it includes timezone information
            if parsed_datetime.tzinfo is not None:
                parsed_datetime = parsed_datetime.replace(tzinfo=None)
            return parsed_datetime
        except ValueError as e:
            print(f"ValueError: {e}")
            continue
    raise ValueError(f"datetime {datetime_str} does not match expected formats.")    

def remove_weighted_mean(data):
    sigma_keys = {'dN': 'sN', 'dE': 'sE', 'dU': 'sU', 'Elevation': 'sElevation'}  # Assume sElevation exists
    for component in ['dN', 'dE', 'dU', 'Elevation']:
        sigma_key = sigma_keys[component]
        weights = 1 / data[sigma_key]**2
        weighted_mean = np.average(data[component], weights=weights)
        data[component] -= weighted_mean  # Demean the series
    return data

def apply_smoothing(data):
    for component in ['dN', 'dE', 'dU', 'Elevation']:
        if args.horz_smoothing and (component == 'dN' or component == 'dE'):
            data[f'Smoothed_{component}'] = lowess(data[component], data['Time'], frac=args.horz_smoothing, return_sorted=False)
        if args.vert_smoothing and (component == 'dU' or component == 'Elevation'):
            data[f'Smoothed_{component}'] = lowess(data[component], data['Time'], frac=args.vert_smoothing, return_sorted=False)
    return data

def compute_statistics(data):
    stats = {}
    for component in ['dN', 'dE', 'dU', 'Elevation']:
        sigma_key = f's{component[-1].upper()}' if component != 'Elevation' else 'sElevation'
        weights = 1 / data[sigma_key]**2

        weighted_mean = np.average(data[component], weights=weights)
        std_dev = np.sqrt(np.average((data[component] - weighted_mean)**2, weights=weights))
        rms = np.sqrt(np.mean(data[component]**2))

        # Store calculated statistics
        stats[component] = {
            'weighted_mean': weighted_mean,
            'std_dev': std_dev,
            'rms': rms
        }

        # Prepare series for plotting
        data[f'{component}_weighted_mean'] = [weighted_mean] * len(data)
        data[f'{component}_std_dev_upper'] = weighted_mean + std_dev*2.0
        data[f'{component}_std_dev_lower'] = weighted_mean - std_dev*2.0

    return data, stats


# Setup and parse arguments
parser = argparse.ArgumentParser(description="Plot positional data with optional smoothing and color coding.")
parser.add_argument('--start-datetime', type=str, 
                    help="Start datetime in the format YYYY-MM-DDTHH:MM:SS, optional timezone")
parser.add_argument('--end-datetime', type=str, 
                    help="End datetime in the format YYYY-MM-DDTHH:MM:SS, optional timezone")
parser.add_argument('--horz_smoothing', type=float, default=None,
                    help='Fraction of the data used for horizontal (East and North) LOWESS smoothing (optional).')
parser.add_argument('--vert_smoothing', type=float, default=None,
                    help='Fraction of the data used for vertical (Up) LOWESS smoothing (optional).')
parser.add_argument('--colour_sigma', action='store_true',
                    help='Colourize the timeseries using the standard deviation (sigma) values (optional).')
parser.add_argument('--max_sigma', type=float, default=None,
                    help='Set a maximum sigma threshold for the sigma colour scale (optional).')
parser.add_argument('--elevation', action='store_true',
                    help='Plot Elevation values inplace of dU wrt the reference coord (optional).')
parser.add_argument('--demean', action='store_true',
                    help='Remove the mean values from all time series before plotting (optional).')
parser.add_argument('--map', action='store_true',
                    help='Create a geographic map view from the Longitude & Latitude estiamtes (optional).')
parser.add_argument('--heatmap', action='store_true',
                    help='Create a 2D heatmap view of E & N coodrinates wrt the reference position  (optional).')
parser.add_argument('--sigma_threshold', nargs=3, type=float, 
                    help="Thresholds for sE, sN, and sU to filter data.")
parser.add_argument('--down_sample', type=int, 
                    help="Interval in seconds for down-sampling data.")
parser.add_argument('--save', action='store_true',
                    help='Save requested plots as .html format files (optional).')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

# Parse the start and end datetime if provided
start_datetime = parse_datetime(args.start_datetime) if args.start_datetime else None
end_datetime = parse_datetime(args.end_datetime) if args.end_datetime else None

# Load and process data
all_data = pd.DataFrame()
for file_path in args.files:
    file_data = parse_pos_format(file_path)
    all_data = pd.concat([all_data, file_data], ignore_index=True)

all_data['Time'] = pd.to_datetime(all_data['Time'], format="%Y-%m-%dT%H:%M:%S.%f")

# Apply time windowing
if start_datetime:
    all_data = all_data[all_data['Time'] >= start_datetime]
if end_datetime:
    all_data = all_data[all_data['Time'] <= end_datetime]

# Apply threshold filtering if sigma_threshold is provided
if args.sigma_threshold:
    se_threshold, sn_threshold, su_threshold = args.sigma_threshold
    mask = (all_data['sE'] <= se_threshold) & (all_data['sN'] <= sn_threshold) & (all_data['sU'] <= su_threshold) & (all_data['sElevation'] <= su_threshold)
    all_data = all_data[mask]    

# Down-sample the data if requested
if args.down_sample:
    # Ensure the 'Time' column is datetime for proper indexing
    all_data['Time'] = pd.to_datetime(all_data['Time'])
    all_data.set_index('Time', inplace=True)
    # Resample and take the first available data point in each bin
    all_data = all_data.resample(f'{args.down_sample}s').first().dropna().reset_index()

# Demean, smooth, and compute statistics
if args.demean:
    all_data = remove_weighted_mean(all_data)
all_data = apply_smoothing(all_data)
all_data, component_stats = compute_statistics(all_data)

# Start plotting
# Determine max sigma and color scale settings for Fig1
title_text = f"<b>Time Series Analysis</b>: {', '.join(args.files)}<br>"
color_scale = 'Jet' if args.colour_sigma else None  # Only set color scale if --colour_sigma is active
max_sigma_data = np.max([all_data['sN'].max(), all_data['sE'].max(), all_data['sU'].max()])
min_sigma_data = np.min([all_data['sN'].min(), all_data['sE'].min(), all_data['sU'].min()])
cmax = min(args.max_sigma, max_sigma_data) if args.max_sigma is not None else max_sigma_data
#cmin = min_sigma_data 
cmin = 0.0 

# Setting up the plot
fig1 = go.Figure()
components = ['dN', 'dE', 'Elevation'] if args.elevation else ['dN', 'dE', 'dU']

for component in components:
    # Correctly map the component to its sigma key
    if component == 'Elevation':
        sigma_key = 'sU'  # Assuming sigma for Elevation is stored in 'sU'
    else:
        sigma_key = f's{component[-1].upper()}'
    
    print('Plotting: ', sigma_key)  # To check if the correct sigma key is being used

    # Add the primary and smoothed series data
    if args.colour_sigma:
        # When using --colour_sigma, use the sigma value for coloring
        fig1.add_trace(go.Scatter(
            x=all_data['Time'], y=all_data[component],
            mode='lines+markers',
            marker=dict(size=5, color=all_data[sigma_key], coloraxis="coloraxis"),
            name=component,
            hoverinfo='text+x+y',
            text=f'{component} Sigma: ' + all_data[sigma_key].astype(str)
        ))

    else:
        # When not using --colour_sigma, add error bars using the sigma values
        fig1.add_trace(go.Scatter(
            x=all_data['Time'], y=all_data[component],
            mode='markers',
            name=component,
            error_y=dict(
                type='data',  # Represent error in data coordinates
                array=all_data[sigma_key],  # Positive error
                arrayminus=all_data[sigma_key],  # Negative error
                visible=True,  # Make error bars visible
                color='gray'  # Color of error bars
            ),
            marker=dict(size=5, color='blue'),
            line=dict(color='blue'),
            hoverinfo='text+x+y',
            text=f'{component} Sigma: ' + all_data[sigma_key].astype(str)
        ))

    if f'Smoothed_{component}' in all_data:
        fig1.add_trace(go.Scatter(
            x=all_data['Time'], y=all_data[f'Smoothed_{component}'],
            mode='lines',
            name=f'Smoothed {component}',
            line=dict(color='rgba(0,0,255,0.5)')
        ))

    # Add statistical lines and shaded areas for standard deviation
    fig1.add_trace(go.Scatter(
        x=all_data['Time'], y=all_data[f'{component}_weighted_mean'],
        mode='lines',
        name=f'{component} Weighted Mean',
        line=dict(color='red')
    ))

    fig1.add_trace(go.Scatter(
        x=all_data['Time'].tolist() + all_data['Time'].tolist()[::-1],
        y=all_data[f'{component}_std_dev_upper'].tolist() + all_data[f'{component}_std_dev_lower'].tolist()[::-1],
        fill='toself',
        fillcolor='rgba(68, 68, 255, 0.2)',
        line=dict(color='rgba(255,255,255,0)'),
        name=f'{component} CI: 2 Sigma (95%)'
    ))

    stats = component_stats[component]
    title_text += f"<b>{component}</b>: Weighted Mean = {stats['weighted_mean']:.3f}, Std Dev = {stats['std_dev']:.3f}, RMS = {stats['rms']:.3f}<br>"

fig1.update_layout(
    title=title_text,
    xaxis_title='Time',
    yaxis_title='Measurement Value',
    xaxis=dict(
        rangeslider=dict(visible=True),
        fixedrange=False,  
        type='date'
    ),
    yaxis=dict(
        fixedrange=False
    ),
    coloraxis=dict(
        colorscale=color_scale,
        cmin=cmin,
        cmax=cmax,
        colorbar=dict(
            title='Sigma Value',
            x=0.5,  # Center the color bar on the x-axis
            y=-0.5,  # Position the color bar below the x-axis
            xanchor='center',  # Anchor the color bar at its center for x positioning
            yanchor='bottom',  # Anchor the color bar from its bottom edge for y positioning
            len=0.5,  # Length of the color bar (75% of the width of the plot area)
            thickness=10,  # Thickness of the color bar
            orientation='h'  # Horizontal orientation
        ),
    ) if args.colour_sigma else {},
    showlegend=True,
    margin=dict(t=150) 
)
fig1.show()
if args.save:
    fig1.write_html("fig1.html")

# Build the title with file names and statistics for Fig2
title_text = f"<b>dN vs dE Analysis</b>: {', '.join(args.files)}<br>"
for component in ['dN', 'dE']:
    stats = component_stats[component]
    title_text += f"<b>{component}</b>: Weighted Mean = {stats['weighted_mean']:.3f}, Std Dev = {stats['std_dev']:.3f}, RMS = {stats['rms']:.3f}<br>"

# Conditional sigma calculations and setup
composite_uncertainty = np.sqrt(all_data['sN']**2 + all_data['sE']**2)
all_data['composite_uncertainty'] = composite_uncertainty
max_sigma_data = composite_uncertainty.max()
if args.colour_sigma:
    cmax = min(args.max_sigma, max_sigma_data) if args.max_sigma is not None else max_sigma_data
    cmin = composite_uncertainty.min()
    #cmin = 0.0
    color_scale = 'Jet'  # Define the color scale here within the condition
else:
    cmin = None  # No cmax needed for static colors
    cmax = None  # No cmax needed for static colors
    color_scale = None  # No color scale needed for static colors

# Plot configuration
fig2 = go.Figure()
fig2.add_trace(go.Scatter(
    x=all_data['dE'], y=all_data['dN'],
    mode='markers',
    marker=dict(
        size=5,
        color=all_data['composite_uncertainty'] if args.colour_sigma else 'blue',  # Conditional coloring
        coloraxis="coloraxis" if args.colour_sigma else None  # Use color axis only if color sigma is set
    ),
    name='dE vs dN',
    text=[f"{time} Sigma dNdE: {unc:.4f}" for time, unc in zip(all_data['Time'], all_data['composite_uncertainty'])],
    hoverinfo='text+x+y'
))

# Add smoothed data if available
if 'Smoothed_dN' in all_data.columns and 'Smoothed_dE' in all_data.columns:
    fig2.add_trace(go.Scatter(
        x=all_data['Smoothed_dE'], y=all_data['Smoothed_dN'],
        mode='markers',
        marker=dict(
            size=5,
            color='red'
        ),
        name='Smoothed'
    ))

# Layout update with conditional color axis settings
fig2.update_layout(
    title=title_text,
    xaxis_title='dE (meters)',
    yaxis_title='dN (meters)',
    xaxis=dict(scaleanchor="y", scaleratio=1),
    yaxis=dict(scaleanchor="x", scaleratio=1),
    coloraxis=dict(
        colorscale=color_scale,
        cmin=cmin,
        cmax=cmax,
        colorbar=dict(
            title='Sigma Value',
            x=0.5, y=-0.15,  # Adjusted for visibility
            xanchor='center', yanchor='bottom',
            len=0.75, thickness=20, orientation='h'
        )
    ) if args.colour_sigma else None,  # Apply color axis settings only if needed
    showlegend=True
)
fig2.show()
if args.save:
    fig2.write_html("fig2.html")

if args.map:
    # Plotly plotting using mapbox open-street-map

    # Adjust the zoom level dynamically based on the spread of the latitude and longitude
    def adjust_zoom(latitudes, longitudes):
        lat_range = np.ptp(latitudes)  # Peak to peak (range) of latitudes
        lon_range = np.ptp(longitudes)  # Peak to peak (range) of longitudes
        if max(lat_range, lon_range) < 0.02:
            return 13  # City level zoom
        elif max(lat_range, lon_range) < 0.1:
            return 10  # Regional level zoom
        elif max(lat_range, lon_range) < 1:
            return 7  # Country level zoom
        else:
            return 5  # Continental level zoom

    zoom_level = adjust_zoom(all_data['Latitude'], all_data['Longitude'])

    fig3 = go.Figure(go.Scattermapbox(
        lat=all_data['Latitude'],
        lon=all_data['Longitude'],
        mode='markers+lines',
        marker=dict(size=5, color='blue')
    ))

    fig3.update_layout(
        mapbox=dict(
            style="open-street-map",
            center=go.layout.mapbox.Center(
                lat=all_data['Latitude'].mean(), 
                lon=all_data['Longitude'].mean()
            ),
            zoom=zoom_level
        ),
        title='Geographic Plot of Latitude and Longitude',
        showlegend=False
    )

    fig3.show()
    if args.save:
        fig2.write_html("fig3.html")

if args.heatmap:
    # Plotly plotting dN vs dE heatmap
    fig4 = go.Figure()
    fig4.add_trace(go.Histogram2dContour(
        x = all_data['dE'],
        y = all_data['dN'],
        colorscale = 'Jet',
        reversescale = False,
        xaxis = 'x',
        yaxis = 'y'
    ))
    fig4.add_trace(go.Scatter(
        x = all_data['dE'],
        y = all_data['dN'],
        xaxis = 'x',
        yaxis = 'y',
        mode = 'markers',
        marker = dict(
            color = 'rgba(0,0,0,0.3)',
            size = 3
        )
    ))
    fig4.add_trace(go.Scatter(
        x = all_data['dE_weighted_mean'],
        y = all_data['dN_weighted_mean'],
        xaxis = 'x',
        yaxis = 'y',
        mode = 'markers',
        marker = dict(
            color="white",
            size = 15,
            line_color='black',
            symbol='x-dot',
            line_width=2
        ),
        hoverinfo='text+x+y',
        text='Weighted Mean (dE, dN)'
    ))
    fig4.add_trace(go.Histogram(
        y = all_data['dN'],
        xaxis = 'x2',
        marker = dict(
            color = 'rgba(0,0,0,1)'
        )
    ))
    fig4.add_trace(go.Histogram(
        x = all_data['dE'],
        yaxis = 'y2',
        marker = dict(
            color = 'rgba(0,0,0,1)'
        )
    ))

    fig4.update_layout(
    autosize = False,
    xaxis = dict(
        zeroline = False,
        domain = [0,0.85],
        showgrid = False
    ),
    yaxis = dict(
        zeroline = False,
        domain = [0,0.85],
        showgrid = False
    ),
    xaxis2 = dict(
        zeroline = False,
        domain = [0.85,1],
        showgrid = False
    ),
    yaxis2 = dict(
        zeroline = False,
        domain = [0.85,1],
        showgrid = False
    ),
    title=title_text,
    xaxis_title='dE (meters)',
    yaxis_title='dN (meters)',
    height = 800,
    width = 800,
    bargap = 0,
    hovermode = 'closest',
    showlegend = False
    )

    fig4.show()
    if args.save:
        fig4.write_html("fig4.html")





