#pragma once
// clang-format off
static const char* htmlFooterTemplate = R"HTMLTEMPLATE(
<div class="toolbar">
	<div class="toolbar-row">
		<input type="file" name="inputfile" id="inputfile">
	</div>
	<div class="toolbar-row">
		<button id="generate" class="primary">Generate yaml</button>
		<button id="create" class="secondary">Save file</button>
	</div>
</div>
<textarea id="textbox" rows="30" cols="200" disabled>generated yaml file</textarea>
</div><!-- .page-content -->
</body>
	<script>
(function ()
	{
		var textFile = null,
		makeTextFile = function (text)
			{
				var data = new Blob([text], {type: 'text/plain'});
				// If we are replacing a previously generated file we need to
				// manually revoke the object URL to avoid memory leaks.
				if (textFile !== null)
				{
					window.URL.revokeObjectURL(textFile);
				}
				textFile = window.URL.createObjectURL(data);
				return textFile;
			};
		var create      = document.getElementById('create');
		var textbox     = document.getElementById('textbox');
		var generate    = document.getElementById('generate');
		var inputfile   = document.getElementById('inputfile');
		var configLevel = document.getElementById('configLevel');
		configLevel.addEventListener('change', function()
			{
				let level = $("#configLevel").val();
				console.log("edit" + level)
				if (level >= 1)     $(".level1").css("display", "block");
				else                $(".level1").css("display", "none");
				if (level >= 2)     $(".level2").css("display", "block");
				else                $(".level2").css("display", "none");
				if (level >= 3)     $(".level3").css("display", "block");
				else                $(".level3").css("display", "none");
			});
		$("#configLevel").val(1);
		generate.addEventListener('click', function()
			{
				var yaml = "";
				var checked = $("input:checked + .ident");
				checked.each(function(index, item)
					{
						var hidden = $(item).parents(":hidden");
						if (hidden.length)
							return;
						var string = $(item).attr("data-indent") + item.textContent.split(/[\s\r\n]+/)[0] + " ";
						var values = $(item).siblings(".value");
						if ($(values).length)
						{
							// var bits = values[0].value;
							string += values[0].value;
						}
						yaml += string + "\n";
						console.log(string);
					});
				$("textarea").val(yaml);
			}, false);
		create.addEventListener('click', function ()
			{
				var link = document.createElement('a');
				link.setAttribute('download', 'output.yaml');
				link.href = makeTextFile(textbox.value);
				document.body.appendChild(link);
				// wait for the link to be added to the document
				window.requestAnimationFrame(function ()
					{
						var event = new MouseEvent('click');
						link.dispatchEvent(event);
						document.body.removeChild(link);
					});
			}, false);
		inputfile.addEventListener('change', function()
			{
				var fr = new FileReader();
				fr.onload = function()
					{
						var doc;
						// Get document, or throw exception on error
						try
						{
							doc = jsyaml.load(fr.result);
						}
						catch (e)
						{
						}
						const isObject = val => val && typeof val === 'object' && !Array.isArray(val);

						// Strings starting with a wildcard ("*") must be quoted or else js-yaml treats them as anchor references
						// Date objects must be placed within quotations
						// Strings containing spaces or colons also get quoted just to be safe
						const formatScalar = function(v)
						{
							if (v === null || v === undefined) return "";
							if (v instanceof Date)
							{
								// Reproduce the original 'YYYY-MM-DD HH:MM:SS' form in single quotes
								var pad = function(n) { return (n < 10 ? "0" : "") + n; };
								var s = v.getUTCFullYear() + "-" + pad(v.getUTCMonth() + 1) + "-" + pad(v.getUTCDate())
									+ " " + pad(v.getUTCHours()) + ":" + pad(v.getUTCMinutes()) + ":" + pad(v.getUTCSeconds());
								return "'" + s + "'";
							}
							if (typeof v === 'string')
							{
								if (v.length === 0) return "";
								var needsQuote = /^[*&!|>%@`]/.test(v) || /[:#]/.test(v);
								if (needsQuote) return "'" + v + "'";
								return v;
							}
							return String(v);
						};
						const formatValue = function(value)
						{
							if (Array.isArray(value))
							{
								return "[" + value.map(formatScalar).join(",") + "]";
							}
							return formatScalar(value);
						};
						// Resolve a full-path id, trying case variants on the final segment
						// The DOM may have e.g. '...rnx_code_conversions:p1:' while the yaml
						// supplies 'P1', so the straight id lookup misses
						const resolvePathId = function(head, key)
						{
							var tries = [key, key.toLowerCase(), key.toUpperCase()];
							for (var i = 0; i < tries.length; i++)
							{
								var id = head + tries[i] + ":";
								var $el = $("#" + CSS.escape(id));
								if ($el.length) return { id: id, $el: $el };
							}
							return { id: head + key + ":", $el: $() };
						};
						// Set a .value sibling. For <select> elements the option values in this
						// DOM may have leading whitespace and differ in case from the yaml enum
						// tokens, so we fall back to a normalised scan over the options.
						const setValue = function($target, formatted, rawValue)
						{
							if (!$target.length) return;
							$target.val(formatted);
							$target.filter("select").each(function()
							{
								var el = this;
								if (el.selectedIndex >= 0 && el.value === formatted && formatted !== "") return;
								if (typeof rawValue !== 'string' || rawValue.length === 0) return;
								var wanted = rawValue.toLowerCase().replace(/[-_\s]/g, '');
								for (var j = 0; j < el.options.length; j++)
								{
									var opt = el.options[j];
									var normVal  = (opt.value || '').toLowerCase().replace(/[-_\s]/g, '');
									var normText = (opt.text  || '').toLowerCase().replace(/[-_\s]/g, '');
									if (normVal === wanted || normText === wanted)
									{
										el.selectedIndex = j;
										return;
									}
								}
							});
						};

						const paths = (obj = {}, head = '', depth = "") =>
						{
							Object.entries(obj).reduce((product, [key, value]) =>
							{
								// Try case variants so that yaml keys like "P1"
								// match DOM ids like "...:p1:".
								var resolved = resolvePathId(head, key);
								var fullPath = resolved.id;
								if (resolved.$el.length)
								{
									resolved.$el.prop("checked", true);
								}
								if (isObject(value))
								{
									paths(value, fullPath, depth + "    ");
								}
								else
								{
									// Use formatValue / setValue for quoting + select fallback
									var $target = resolved.$el.siblings(".value");
									setValue($target, formatValue(value), value);
								}
							}, []);
						}

                        // Generic helper: clone a template element whose DOM id contains
                        // 'templateKey' into siblings for each name in 'targetNames',
                        // rewriting ids, for-attrs, and visible text
                        var cloneTemplateBlock = function(templateIdPrefix, templateKey, targetNames)
                        {
                            var templateSelector = "#" + templateIdPrefix.replace(/:/g, "\\:") + templateKey + "\\:";
                            var $template = $(templateSelector).closest('.element');
                            if (!$template.length) return;
                            targetNames.forEach(function(name)
                            {
                                if (name === templateKey) return;
                                var existingSelector = "#" + templateIdPrefix.replace(/:/g, "\\:") + name + "\\:";
                                if ($(existingSelector).length) return;
                                var $clone = $template.clone(true);
                                var keyRe = new RegExp(templateKey, 'g');
                                $clone.find('[id]').addBack('[id]').each(function() {
                                    $(this).attr('id', $(this).attr('id').replace(keyRe, name));
                                });
                                $clone.find('[for]').each(function() {
                                    $(this).attr('for', $(this).attr('for').replace(keyRe, name));
                                });
                                var $ident = $clone.find('.ident').first();
                                $ident.find('b').each(function() {
                                    $(this).text($(this).text().replace(templateKey, name));
                                });
                                $ident.contents().filter(function() {
                                    return this.nodeType === 3;
                                }).each(function() {
                                    this.textContent = this.textContent.replace(templateKey, name);
                                });
                                $template.after($clone);
                            });
                        };

                        // Frequency code candidates used to probe for placeholder template blocks
                        var frequencyCandidates = ['l1w','l1c','l2w','l2l','l5q','l1p','l2p','l2c','l6c','l7q','l2i','l7i','l6i','l5p'];

                        // Helper: under a given parent prefix, clone the placeholder code-frequency
                        // template block (e.g. ':l1w:') for each requested code frequency name
                        var cloneFrequencyBlocks = function(prefix, codeFrequencyNames)
                        {
                            var templateCodeFrequency = null;
                            for (var i = 0; i < frequencyCandidates.length; i++)
                            {
                                if ($("#" + prefix.replace(/:/g, "\\:") + frequencyCandidates[i] + "\\:").length)
                                {
                                    templateCodeFrequency = frequencyCandidates[i];
                                    break;
                                }
                            }
                            if (!templateCodeFrequency) return;
                            cloneTemplateBlock(prefix, templateCodeFrequency, codeFrequencyNames);
                        };

                        // Clone constellation blocks under receiver_options.global from the gps template
                        // so that glo / gal / bds / qzs rinex2 conversions can be filled in
                        if (doc.receiver_options && doc.receiver_options.global)
                        {
                            var receiverConstellations = Object.keys(doc.receiver_options.global)
                                .filter(function(k) { return ['gps','glo','gal','bds','qzs'].indexOf(k) !== -1; });
                            cloneTemplateBlock("receiver_options:global:", "gps", receiverConstellations);
                        }

                        // Clone estimation_parameters.receivers.<marker_name> blocks from the xmpl template
                        // (mirrors the receiver_options.<marker_name> patch below)
                        if (doc.estimation_parameters && doc.estimation_parameters.receivers)
                        {
                            var estReceiverMarkerNames = Object.keys(doc.estimation_parameters.receivers)
                                .filter(function(k) { return k !== 'global' && k !== 'xmpl'; });
                            cloneTemplateBlock("estimation_parameters:receivers:", "xmpl", estReceiverMarkerNames);
                        }

                        // Clone estimation_parameters.receivers.<marker_name>.<constellation>.<code_frequency> blocks
                        // so that e.g. gps.l5q can be filled in from an l1w template
                        // Walks 'global' and any marker_name entries present in the imported yaml
                        if (doc.estimation_parameters && doc.estimation_parameters.receivers)
                        {
                            var estReceivers = doc.estimation_parameters.receivers;
                            Object.keys(estReceivers).forEach(function(markerName) {
                                var markerObj = estReceivers[markerName];
                                if (!markerObj || typeof markerObj !== 'object') return;
                                ['gps','glo','gal','bds','qzs'].forEach(function(constellation) {
                                    var constellationObj = markerObj[constellation];
                                    if (!constellationObj || typeof constellationObj !== 'object') return;
                                    var codeFrequencies = Object.keys(constellationObj);
                                    if (!codeFrequencies.length) return;
                                    var prefix = "estimation_parameters:receivers:" + markerName + ":" + constellation + ":";
                                    cloneFrequencyBlocks(prefix, codeFrequencies);
                                });
                            });
                        }

                        // Clone constellation-level blocks under satellite_options
                        // Constellations sit as siblings of 'global' (e.g. satellite_options.gps,
                        // satellite_options.gal). Satellite SVN / PRN keys (e.g. g--, g01) also sit
                        // here and use the same template shape
                        if (doc.satellite_options)
                        {
                            var satOptionGroups = Object.keys(doc.satellite_options)
                                .filter(function(k) { return k !== 'global'; });
                            cloneTemplateBlock("satellite_options:", "global", satOptionGroups);
                        }

                        // Clone code frequency blocks under each satellite_options.<group>.
                        // Code frequencies (l1w, l5q, ...) sit directly under the group (global, gps, ...),
                        // not under a separate constellation layer
                        if (doc.satellite_options)
                        {
                            Object.keys(doc.satellite_options).forEach(function(group) {
                                var groupObj = doc.satellite_options[group];
                                if (!groupObj || typeof groupObj !== 'object') return;
                                var codeFrequencies = Object.keys(groupObj).filter(function(k) {
                                    return frequencyCandidates.indexOf(k.toLowerCase()) !== -1;
                                });
                                if (!codeFrequencies.length) return;
                                cloneFrequencyBlocks("satellite_options:" + group + ":", codeFrequencies);
                            });
                        }

                        // Clone constellation/SVN blocks under estimation_parameters.satellites
                        if (doc.estimation_parameters && doc.estimation_parameters.satellites)
                        {
                            var estSatGroups = Object.keys(doc.estimation_parameters.satellites)
                                .filter(function(k) { return k !== 'global'; });
                            cloneTemplateBlock("estimation_parameters:satellites:", "global", estSatGroups);
                        }

                        // Clone code frequency blocks under each estimation_parameters.satellites.<group>.
                        if (doc.estimation_parameters && doc.estimation_parameters.satellites)
                        {
                            Object.keys(doc.estimation_parameters.satellites).forEach(function(group) {
                                var groupObj = doc.estimation_parameters.satellites[group];
                                if (!groupObj || typeof groupObj !== 'object') return;
                                var codeFrequencies = Object.keys(groupObj).filter(function(k) {
                                    return frequencyCandidates.indexOf(k.toLowerCase()) !== -1;
                                });
                                if (!codeFrequencies.length) return;
                                cloneFrequencyBlocks("estimation_parameters:satellites:" + group + ":", codeFrequencies);
                            });
                        }

						// Clone the "xmpl" template block for each non-global marker name in receiver_options
                        if (doc.receiver_options) {
                            Object.keys(doc.receiver_options).forEach(function(markerName) {
                                if (markerName === 'global') return;

                                if ($("#receiver_options\\:" + markerName + "\\:").length) return;

                                // Find the xmpl template block and deep clone it
                                var $template = $("#receiver_options\\:xmpl\\:").closest('.element');
                                if (!$template.length) return;

                                var $clone = $template.clone(true);

                                // Replace all id and for attributes containing 'xmpl' with the marker name
                                $clone.find('[id]').addBack('[id]').each(function() {
                                    $(this).attr('id', $(this).attr('id').replace(/xmpl/g, markerName));
                                });
                                $clone.find('[for]').each(function() {
                                    $(this).attr('for', $(this).attr('for').replace(/xmpl/g, markerName));
                                });

                                // Update the visible label text (may be in a <b> tag or a text node)
                                var $ident = $clone.find('.ident').first();
                                $ident.find('b').each(function() {
                                    $(this).text($(this).text().replace('xmpl', markerName));
                                });
                                $ident.contents().filter(function() {
                                    return this.nodeType === 3;
                                }).each(function() {
                                    this.textContent = this.textContent.replace('xmpl', markerName);
                                });

                                // Insert the clone after the xmpl block
                                $template.after($clone);
                            });
                        }

                        // Clone outputs.streams.<marker_name> blocks from the xmpl template
                        if (doc.outputs && doc.outputs.streams)
                        {
                            var streamMarkerNames = Object.keys(doc.outputs.streams)
                                .filter(function(k) { return k !== 'xmpl'; });
                            cloneTemplateBlock("outputs:streams:", "xmpl", streamMarkerNames);
                        }

						paths(doc);
					}
				fr.readAsText(this.files[0]);
			});
	})();
	</script>
</html>
)HTMLTEMPLATE";
// clang-format on