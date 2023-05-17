


var enc = new TextDecoder("utf-8");

var map;

var mapCallback;
var mapInit = function()
{

	mapboxgl.accessToken = 'pk.eyJ1IjoicG9seXRvcGVzLWRlc2lnbiIsImEiOiJjbDZ3MDE2YXMyZGJiM2p0ZXNmcTZzM2c3In0.SFR30kmWqzxval3urUviww';

	map = new mapboxgl.Map(
	{
		container: 'map',
// 		style: 'mapbox://styles/mapbox/dark-v9',
		style: 'mapbox://styles/mapbox/light-v9',
		// style: 'mapbox://styles/mapbox/satellite-streets-v11',
		// style: 'mapbox://styles/mapbox-map-design/ckhqrf2tz0dt119ny6azh975y',
		// pitch: 45,
		center: [0, 0],
		maxBounds: 
			[
				[-360, -85],
				[360, 85]
			],
			zoom: 1
	});	
	
	
	map.on('load', function()
	{
		const layers = map.getStyle().layers;

		if (false)
		{
			const labelLayerId = layers.find(
				(layer) => layer.type === 'symbol' && layer.layout['text-field']
			).id;

			map.addLayer(
			{
				'id': 'add-3d-buildings',
				'source': 'composite',
				'source-layer': 'building',
				'filter': ['==', 'extrude', 'true'],
				'type': 'fill-extrusion',
				'minzoom': 15,
				'paint': 
				{
					'fill-extrusion-color': '#aaa',
					
					// Use an 'interpolate' expression to
					// add a smooth transition effect to
					// the buildings as the user zooms in.
					'fill-extrusion-height':
					[
						'interpolate',
						['linear'],
						['zoom'],
						15,
						0,
						15.05,
						['get', 'height']
					],
					'fill-extrusion-base': 
					[
						'interpolate',
						['linear'],
						['zoom'],
						15,
						0,
						15.05,
						['get', 'min_height']
					],
					'fill-extrusion-opacity': 0.6
				}
			}, labelLayerId);
		}	
		
		map.addControl(new mapboxgl.FullscreenControl());

// 		document.getElementById('locate').addEventListener('click', function(e)
// 		{
// 			var lastSeenLocaton = JSON.parse(this.getAttribute('data-coordinate'));
// 			console.log(lastSeenLocaton);
// 			map.flyTo(
// 			{
// 				center: lastSeenLocaton
// 			});
// 		});


		const draw = new MapboxDraw(
		{
			displayControlsDefault: false,
			// Select which mapbox-gl-draw control buttons to add to the map.
			controls: 
			{
				polygon: true,
				trash: true
			},
			// Set mapbox-gl-draw to draw by default.
			// The user does not have to click the polygon control button first.
			defaultMode: 'draw_polygon'
		});
		
		map.addControl(draw);
	
		map.on('draw.create', updateArea);
		map.on('draw.delete', updateArea);
		map.on('draw.update', updateArea);
	

		function updateArea(e) 
		{
			const data = draw.getAll();
			
			if (data.features.length > 0) 
			{
				console.log(data.features[0].geometry.coordinates);
			}
			return;

			const answer = document.getElementById('calculated-area');

			if (data.features.length > 0) 
			{
			
				const area = turf.area(data);
				// Restrict the area to 2 decimal points.
				const rounded_area = Math.round(area * 100) / 100;
				answer.innerHTML = `<p><strong>${rounded_area}</strong></p><p>square meters</p>`;
			} 
			else 
			{
				answer.innerHTML = '';
				if (e.type !== 'draw.delete')
					alert('Click the map to draw a polygon.');
			}
		}

	
		// Create a popup, but don't add it to the map yet.
		const popup = new mapboxgl.Popup(
		{
			closeButton:	false,
			closeOnClick:	false
		});

		mapCallback = function(data)
		{
			if (data.send != "POS")
				return;
			
			var site = data.name;
			
			if (!(site in watchedSites))
			{
				//on site published, create an icon and path
				//subscribe to the specific site
				
				watchedSites[site] = {};
				watchedSites[site].icon = 
				{
					'type': 'FeatureCollection',
					'features': 
					[
						{
							// feature for Mapbox DC
							'type': 'Feature',
							'geometry': 
							{
								'type': 'Point',
								'coordinates': [-77.03238901390978, 38.913188059745586]
							},
							'properties': 
							{
								'title': 'Mapbox DC'
							}
						}
					]
				};
				
				watchedSites[site].path = 
				{
					"type": "FeatureCollection",
					"features": 
					[
					]
				};
				
				map.addSource(site + "_icon",	{ type: 'geojson', data: watchedSites[site].icon });
				map.addSource(site + "_path",	{ type: 'geojson', data: watchedSites[site].path });
				
		
				map.addLayer(
				{
					"id": site + "_icon",
					"type": "circle",
					"source": site + "_icon",
				
					"paint": 
					{
						"circle-radius": 5,
						"circle-color": 'blue'
					},
// 					"layout": 
// 					{
// 						"icon-image": "circle-stroked-15",//"rocket-15",
// 						'icon-rotate': ['get', 'rotation']
// 					}
				});

				map.addLayer(
				{
					'id': site + "_path",
					'type': 'line',
					'source': site + "_path",
					'paint': 
					{
						'line-color': 'blue',
						'line-opacity': 0.75,
						'line-width': 1
					}
				});
				
				map.on('mouseenter', site + "_icon", (e) => 
				{
					// Change the cursor style as a UI indicator.
					map.getCanvas().style.cursor = 'pointer';
				
					// Copy coordinates array.
					const coordinates = e.features[0].geometry.coordinates.slice();
					const description = e.features[0].properties.description;
				
					// Ensure that if the map is zoomed out such that multiple
					// copies of the feature are visible, the popup appears
					// over the copy being pointed to.
					while (Math.abs(e.lngLat.lng - coordinates[0]) > 180) 
					{
						coordinates[0] += e.lngLat.lng > coordinates[0] ? 360 : -360;
					}
				
					// Populate the popup and set its coordinates
					// based on the feature found.
					popup.setLngLat(coordinates).setHTML(description).addTo(map);
				});
				
				map.on('mouseleave', site + "_icon", () => 
				{
					map.getCanvas().style.cursor = '';
					popup.remove();
				});
			}
			
			var issLastSeen		= data.coords;
// 			var rotation		= data.rotation;
			var details			= data;
			var resultingDOM	= "<h4>" + site + "</h4>";
				
			for (var prop in details)
			{
				resultingDOM += "<span class='title'>" + prop.toUpperCase() + "</span>" + " " + details[prop] + "</br>";
			}

// 			document.getElementById('locate').setAttribute("data-coordinate", JSON.stringify(issLastSeen));

			if (!watchedSites[site].initialised)
			{
				watchedSites[site].initialised = true;
				for (var i = 0; i < n; i++)
				{
					var emptyFeature = 
					{
						"type": "Feature",
						"geometry": 
						{
							"type": "LineString",
							"coordinates": [      ] 
						}
					};
					watchedSites[site].path.features.push(emptyFeature)
				}
				
				watchedSites[site].firstSeen = issLastSeen;
			}
			
			watchedSites[site].path.features[0].geometry.coordinates.push(issLastSeen);
			
			watchedSites[site].icon.features[0].geometry.coordinates	= issLastSeen;
// 			watchedSites[site].icon.features[0].properties.rotation		= rotation - 45;
			watchedSites[site].icon.features[0].properties.description	= resultingDOM;

			updateSite(site);
			
			map.getSource(site + "_icon").setData(watchedSites[site].icon);
		}
		
		socketCallbacks.push(mapCallback);
		console.log(mapCallback);
// 		document.getElementById('slider').addEventListener('input', (e) => 
// 		{
// 			percentage = parseInt(e.target.value, 10);
// 			
// 			for (site in watchedSites)
// 			{
// 				updateSite(site);
// 			}
// 		});
// 		
// 		document.getElementById('scaler').addEventListener('input', (e) => 
// 		{
// 			scale = Math.exp(parseFloat(e.target.value, 10));
// 			
// 			for (site in watchedSites)
// 			{
// 				updateSite(site);
// 			}
// 		});
	});	

}

var watchedSites = {};
var percentage = 1000;
var scale = 1;
var n = 1;

function updateSite(site)
{
	var dataSubset = structuredClone(watchedSites[site].path)
	
	for (var i = 0; i < n; i++)
	{
		dataSubset.features[i].geometry.coordinates = dataSubset.features[i].geometry.coordinates.map(
			x => lever(x, watchedSites[site].firstSeen, scale)
																								  );
		
		var newlenght = Math.round(dataSubset.features[i].geometry.coordinates.length * percentage / 1000);
		if (newlenght == 0)
		{
			newlenght = 1;
		}
		dataSubset.features[i].geometry.coordinates.length = newlenght;
	}
	
	map.getSource(site + "_path").setData(dataSubset);
}

function lever(coordinates, firstCoords, scale)
{
	return [firstCoords[0] + (coordinates[0] - firstCoords[0]) * scale,
			firstCoords[1] + (coordinates[1] - firstCoords[1]) * scale];
}

