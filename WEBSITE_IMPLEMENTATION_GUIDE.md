# ClimaSus4R Website Implementation Guide

## Overview

A modern, multilingual website has been successfully created for the ClimaSus4R project using `pkgdown` and GitHub Pages. The implementation includes elegant language selection cards, advanced CSS styling with animations, and automated deployment via GitHub Actions.

## What Has Been Implemented

### 1. Folder Structure

The following directory structure has been created:

```
climasus4r/
├── vignettes-pt/          # Portuguese content
│   ├── about.Rmd
│   └── tutorials.Rmd
├── vignettes-en/          # English content
│   ├── about.Rmd
│   └── tutorials.Rmd
├── vignettes-es/          # Spanish content
│   ├── about.Rmd
│   └── tutorials.Rmd
├── pkgdown/
│   └── assets/
│       └── custom.css     # Modern CSS with animations
├── _pkgdown-pt.yml        # Portuguese site configuration
├── _pkgdown-en.yml        # English site configuration
├── _pkgdown-es.yml        # Spanish site configuration
├── index.html             # Landing page with language cards
└── .github/
    └── workflows/
        └── pkgdown.yaml   # Automated deployment workflow
```

### 2. Landing Page (index.html)

A beautiful landing page featuring:
- **Four elegant cards** for language selection (PT, EN, ES) and AI Assistant
- **Modern gradient background** (purple gradient)
- **Hover animations** with smooth transitions
- **Responsive design** that works on all devices
- **Font Awesome icons** for visual appeal

### 3. Custom CSS (pkgdown/assets/custom.css)

Advanced styling includes:
- **Google Fonts integration** (Montserrat)
- **Card hover effects** with transform and shadow animations
- **Smooth transitions** (0.3s ease-in-out)
- **AOS (Animate On Scroll)** preparation for future enhancements
- **Color-coded icons** for each language

### 4. pkgdown Configuration Files

Three separate configuration files for each language:

**_pkgdown-pt.yml** (Portuguese):
- Navigation menu: Início, Sobre, Tutoriais, Estudos de Caso, Comunidade, Referências, Notícias
- Bootstrap 5 with Cosmo theme
- Custom CSS integration

**_pkgdown-en.yml** (English):
- Navigation menu: Home, About, Tutorials, Case Studies, Community, Reference, News
- Same modern styling as Portuguese version

**_pkgdown-es.yml** (Spanish):
- Navigation menu: Inicio, Sobre, Tutoriales, Estudios de Caso, Comunidad, Referencia, Noticias
- Consistent design across all languages

### 5. Content Structure

Initial content files created for each language:

**about.Rmd**: Information about the project, team, and partners
**tutorials.Rmd**: Hub page with cards linking to individual tutorials

### 6. GitHub Actions Workflow

Automated deployment workflow that:
1. Builds the site for each language (PT, EN, ES)
2. Copies vignettes to the appropriate folders
3. Runs `pkgdown::build_site()` for each language
4. Organizes output into `/pt/`, `/en/`, `/es/` directories
5. Copies the landing page to the root
6. Deploys everything to GitHub Pages

## Next Steps to Complete the Implementation

### Step 1: Add the GitHub Actions Workflow File Manually

Since the GitHub App doesn't have permission to create workflow files, you need to add it manually:

1. Create the file `.github/workflows/pkgdown.yaml` in your local repository
2. Copy the content from the workflow file that's already in the repository
3. Commit and push from your local machine

### Step 2: Enable GitHub Pages

1. Go to your repository on GitHub: `https://github.com/ByMaxAnjos/climasus4r`
2. Click on **Settings** → **Pages**
3. Under **Source**, select **Deploy from a branch**
4. Select branch: **gh-pages** and folder: **/ (root)**
5. Click **Save**

### Step 3: Merge the Branch

The changes are currently in the `website-multilingual` branch. To merge:

```bash
git checkout master
git merge website-multilingual
git push origin master
```

Or create a Pull Request on GitHub and merge it through the web interface.

### Step 4: Add More Content

Expand the content by creating additional vignettes:

**For Tutorials:**
- `quickstart.Rmd` - Quick start guide
- `time-series.Rmd` - Time series analysis tutorial
- `data-import.Rmd` - Data import tutorial

**For Case Studies:**
- Create a `case-studies.Rmd` hub page
- Add individual case study files

### Step 5: Integrate the AI Assistant

To integrate your AI Assistant chatbot:

1. Get the embed code from your chatbot provider
2. Add it to the `index.html` file in the `<script>` section
3. Update the click handler for the AI Assistant card

Example integration:
```javascript
document.getElementById('ai-assistant-card').addEventListener('click', function(e) {
    e.preventDefault();
    // Your chatbot initialization code here
    window.openChatbot(); // or whatever your chatbot's API provides
});
```

### Step 6: Customize the Design

The CSS file is ready for further customization:
- Adjust colors in `custom.css` to match your brand
- Add more animations using AOS library
- Customize card styles and hover effects

## File Locations

All implementation files are located in:
- **Repository**: `/home/ubuntu/climasus4r/`
- **Branch**: `website-multilingual`
- **GitHub URL**: `https://github.com/ByMaxAnjos/climasus4r/tree/website-multilingual`

## Expected Site Structure After Deployment

```
https://bymaxanjos.github.io/climasus4r/
├── index.html                    # Landing page with cards
├── pt/                          # Portuguese site
│   ├── index.html
│   ├── articles/
│   │   ├── about.html
│   │   └── tutorials.html
│   └── reference/
├── en/                          # English site
│   ├── index.html
│   ├── articles/
│   │   ├── about.html
│   │   └── tutorials.html
│   └── reference/
└── es/                          # Spanish site
    ├── index.html
    ├── articles/
    │   ├── about.html
    │   └── tutorials.html
    └── reference/
```

## Key Features

✅ **Single Repository**: All languages in one repo
✅ **Modern Design**: Gradient backgrounds, card animations, custom fonts
✅ **Multilingual**: Full support for PT, EN, ES
✅ **Automated Deployment**: GitHub Actions handles everything
✅ **Community Integration**: Direct links to GitHub Issues and Discussions
✅ **AI Assistant Ready**: Placeholder for chatbot integration
✅ **Responsive**: Works on desktop, tablet, and mobile
✅ **Maintainable**: Easy to update content by editing .Rmd files

## Technical Stack

- **Generator**: pkgdown (R)
- **Styling**: Bootstrap 5 + Custom CSS
- **Fonts**: Google Fonts (Montserrat)
- **Icons**: Font Awesome 6
- **Deployment**: GitHub Actions + GitHub Pages
- **Version Control**: Git + GitHub

## Support and Documentation

For questions or issues:
- **GitHub Issues**: https://github.com/ByMaxAnjos/climasus4r/issues
- **GitHub Discussions**: https://github.com/ByMaxAnjos/climasus4r/discussions

---

**Implementation Status**: ✅ Complete (pending manual workflow file addition and GitHub Pages configuration)
