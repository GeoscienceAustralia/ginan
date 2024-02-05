 
# Documentation

The html documentation is set up to use client-side processes to simulate a more extensive server.
What this means is that the source for pages is separated and spread across multiple files to enable consistent styling and ease of editing.

## URLS

Pages are all accessed through one URL, with a parameter passed to the page to indicate which content is desired.

    page.html?p=home.md
    
This will load the `home.md` page into the browser. Other pages are accessible by changing the parameter value after the `?p=`

## File Structure

The content is spread across different types of files, which are individually simple and serve a single purpose each.

### The HTML layout - page.html

This file contains the html layout to be used for all pages in the site. 
Things such as the header and footer, dropdown links and all of the scripts and CSS includes are placed in this file.

### Markdown Content ----.md

The content for pages is written in markdown, with the ability to add inline mathematical equations using MathJax.

### HTML Content ----.html

Html fragments may be used in place of markdown, but is not recommended due to otherwise enforced commonality between structures and formatting.

Html content files should ~not~ contain `<html><head><script><body>` tags, as these are pre-populated.

Html content files must have the suffix `.html` to be processed correctly.

## Editing markdown content

While editing markdown is relatively straightforward, it is useful to preview the actual rendered html output before committing changes and pushing back to GitHub. 
The client-side script of this system does not work on modern browsers locally, unless a small http server is simply deployed.

    cd docs
    python3 -m http.server
    
or, by using Node.js:

    cd docs
    http-server

These commands should start a server and provide a link that can be used to access the preview of the rendered page.

Other IDEs will also be able to give a simplified rendering of the output, but without any CSS or MathJax support.

## Publishing to gh-pages

The addition of a GitHub workflow in the repository - `.github/workflows/publish.yaml` - means that any time a commit is pushed to `main`, the content of the repo's `docs` directory will be moved to the `gh-pages` branch, replacing any existing content.
This ensures that the documentation available online is kept synchronised with the correct version of the code.
The content available on the gh-pages website will always be up to date with the latest updates, but the synchronised history of all documentation for each version of the code will be available in the source files. (Like the file you're reading right now)

### Code documentation

The documentation that is written inline within the software's source files will also be copied to the `gh-pages` branch.
The publish workflow will run `doxygen` which will scrape the code for comments and create indexes and call-graphs for all relevant functions.
This documentation will be created within the `codeDocs/` folder automatically, and will be available on the `gh-pages` site.


### Page Indexes ----.index

These are simple text files that are used to concatenate different source files, and specify the different pages that can be accessed.
They are simply a list of paths to markdown files.
There should be one file per line - ensure there is no empty lines in the file.

The more complicated page `peaUsage.index` merges several markdown files into a single page. Its contents are simply:

    usingThePea.md
    peaExamples.md
    peaConfig.md
    