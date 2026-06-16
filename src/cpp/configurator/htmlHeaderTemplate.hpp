#pragma once
// clang-format off
static const char* htmlHeaderTemplate = R"HTMLTEMPLATE(
<html>
<head><title>Ginan YAML Inspector</title>
	<script src="https://code.jquery.com/jquery-3.6.0.min.js" integrity="sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=" crossorigin="anonymous"></script>
	<script src="https://unpkg.com/js-yaml@4.1.0/dist/js-yaml.min.js"></script>
	<link rel="preconnect" href="https://fonts.googleapis.com">
	<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
	<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&display=swap" rel="stylesheet">
	<style>
	:root
	{
		--base-blue: #2c5d7c;
		--dark-blue: #1a3a4d;
		--blue-hover: #234a5f;
		--blue-light: #35729a;
		--bg: #f6f8fa;
		--bg-card: #ffffff;
		--border: #d0d7de;
		--border-light: #e4e8ec;
		--text: #1a1a1a;
		--text-muted: #5a6672;
		--text-disabled: #9aa5b1;
		--accent-light: #e8f0f6;
		--tooltip-bg: var(--dark-blue);
		--radius: 4px;
	}
	*
	{
		box-sizing: border-box;
	}
	body
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 13px;
		line-height: 1.6;
		color: var(--text);
		background: var(--bg);
		margin: 0;
		padding: 0;
	}
	/* Top bar */
	.header-bar
	{
		background: var(--dark-blue);
		padding: 16px 32px;
	}
	.header-bar h1
	{
		font-family: 'IBM Plex Mono', monospace;
		font-weight: 700;
		font-size: 18px;
		color: #ffffff;
		margin: 0;
		letter-spacing: 2px;
		text-transform: uppercase;
	}
	/* Page content below bar */
	.page-content
	{
		padding: 20px 32px;
	}
	.page-content > p
	{
		color: var(--text-muted);
		margin: 2px 0;
		font-size: 12px;
	}
	/* Toolbar area */
	.toolbar
	{
		display: flex;
		flex-direction: column;
		gap: 10px;
		margin: 16px 0;
		padding: 14px 16px;
		background: var(--bg-card);
		border: 1px solid var(--border-light);
		border-radius: var(--radius);
	}
	.toolbar-row
	{
		display: flex;
		align-items: center;
		gap: 10px;
		flex-wrap: wrap;
	}
	/* Buttons */
	button, .btn
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 12px;
		font-weight: 500;
		border: none;
		border-radius: var(--radius);
		padding: 6px 14px;
		cursor: pointer;
		transition: background 0.15s ease, box-shadow 0.15s ease;
	}
	button.primary, #generate
	{
		background: var(--base-blue);
		color: #fff;
	}
	button.primary:hover, #generate:hover
	{
		background: var(--blue-hover);
		box-shadow: 0 1px 3px rgba(0,0,0,0.15);
	}
	button.secondary, #create
	{
		background: var(--accent-light);
		color: var(--dark-blue);
		border: 1px solid var(--border);
	}
	button.secondary:hover, #create:hover
	{
		background: #dae6ef;
		box-shadow: 0 1px 3px rgba(0,0,0,0.1);
	}
	/* File input */
	input[type=file]
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 12px;
		color: var(--text-muted);
	}
	input[type=file]::file-selector-button
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 12px;
		font-weight: 500;
		background: var(--base-blue);
		color: #fff;
		border: none;
		border-radius: var(--radius);
		padding: 6px 14px;
		cursor: pointer;
		margin-right: 10px;
		transition: background 0.15s ease;
	}
	input[type=file]::file-selector-button:hover
	{
		background: var(--blue-hover);
	}
	/* Selects */
	select
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 12px;
		border: 1px solid var(--border);
		border-radius: var(--radius);
		padding: 4px 8px;
		background: var(--bg-card);
		color: var(--text);
		cursor: pointer;
	}
	select:focus
	{
		outline: 2px solid var(--base-blue);
		outline-offset: -1px;
	}
	#configLevel
	{
		font-weight: 500;
	}
	/* Text inputs */
	input[type=text]
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 13px;
		border: none;
		border-bottom: 1px solid var(--border-light);
		width: 70vw;
		padding: 2px 4px;
		color: var(--text);
		background: transparent;
		transition: border-color 0.15s ease;
	}
	input[type=text]:focus
	{
		outline: none;
		border-bottom-color: var(--base-blue);
	}
	/* Textarea */
	textarea
	{
		font-family: 'IBM Plex Mono', monospace;
		font-size: 12px;
		width: 100%;
		max-width: 100%;
		border: 1px solid var(--border);
		border-radius: var(--radius);
		padding: 12px;
		background: var(--bg-card);
		color: var(--text);
		resize: vertical;
		margin-top: 8px;
	}
	/* Checkboxes */
	input[type=checkbox]
	{
		accent-color: var(--base-blue);
		margin-right: 4px;
		cursor: pointer;
	}
	/* Config tree */
	.element
	{
		margin: 1px 0;
		padding: 1px 0;
	}
	.ident
	{
		position: relative;
		display: inline-block;
		border-bottom: 1px dotted var(--border);
		padding: 1px 2px;
		cursor: default;
	}
	.ident b
	{
		color: var(--dark-blue);
		font-weight: 600;
	}
	.contents
	{
		margin-left: 2em;
		padding-left: 8px;
		border-left: 1px solid var(--border-light);
	}
	.value
	{
		display: inline-block;
		-webkit-user-modify: read-write;
		-moz-user-modify: read-write;
		user-modify: read-write;
	}
	/* Disabled/unchecked states */
	input:not(:checked) ~ div
	{
		color: var(--text-disabled);
	}
	input:not(:checked) ~ input
	{
		pointer-events: none;
		color: var(--text-disabled);
	}
	input:not(:checked) ~ select
	{
		pointer-events: none;
		color: var(--text-disabled);
	}
	input:not(:checked) ~ .contents
	{
		display: none;
	}
	input:not(:checked) + div::before
	{
		content: "#";
		color: var(--text-disabled);
	}
	/* Level visibility */
	.level2, .level3, .level4
	{
		display: none;
	}
	.level2:has(.level1), .level3:has(.level2, .level1)
	{
		/* placeholder for future use */
	}
	/* Tooltips */
	.ident .tooltiptext
	{
		visibility: hidden;
		width: 50vw;
		max-width: 600px;
		background-color: var(--tooltip-bg);
		color: #fff;
		text-align: left;
		border-radius: var(--radius);
		padding: 8px 12px;
		font-size: 12px;
		line-height: 1.5;
		position: absolute;
		z-index: 1;
		top: -5px;
		left: 110%;
		box-shadow: 0 4px 12px rgba(0,0,0,0.2);
	}
	.ident .tooltiptext::after
	{
		content: "";
		position: absolute;
		top: 50%;
		right: 100%;
		margin-top: -5px;
		border-width: 5px;
		border-style: solid;
		border-color: transparent var(--tooltip-bg) transparent transparent;
	}
	.ident:hover .tooltiptext
	{
		visibility: visible;
	}
	</style>
</head>
<body>
	<div class="header-bar">
		<h1>Ginan YAML Inspector</h1>
	</div>
	<div class="page-content">
	<p>Use the checkboxes to enable editing and modification of options.
	<p>Existing yaml files and their configuration can be loaded by importing them below.
	<p>
		<select id="configLevel" class="value">
			<option value="1" selected>Basic</option>
			<option value="2">Intermediate</option>
			<option value="3">Advanced</option>
			<option value="4">Debug</option>
		</select>
)HTMLTEMPLATE";
// clang-format on