# Lake Windermere Eutrophication Analysis (2017–2022)

This repository contains the full codebase used in my MSc dissertation project titled:  
**"A Satellite-based Approach to Investigating Eutrophication in Lakes Receiving Wastewater Treatment Effluent"**

The project focuses on Lake Windermere and investigates chlorophyll-a distribution (as a proxy for eutrophication) using satellite remote sensing and correlation with phosphorus and overflow data from nearby wastewater treatment sources.

---

## 📁 Repository Structure

├── ndci_sectional_analysis.R # NDCI trend across lake sections (2017–2022)
├── ndci_yearly_trend_analysis.R # Year-wise NDCI analysis across sections
├── phosphorus_overflow_correlation.R # Correlation of NDCI with phosphorus & overflow
├── utils.R # Shared helper functions
├── s2_ndwi_ndci_processing.js # GEE script for pansharpening, cloud-masking, NDWI/NDCI generation
├── data/
│ └── NDCI_x/ # Subfolders for each section with raster images
├── outputs/ # Results: figures, Excel files, summaries
└── README.md # This file

---

## 🧪 Tools Used

- **Google Earth Engine (GEE)** for image filtering, NDWI and NDCI generation
- **R (RStudio)** for all analysis, statistical testing, and visualisation  
  Key packages:
  - `tidyverse`, `ggplot2`, `ggpmisc`, `EnvStats`, `rstatix`, `openxlsx`, `httr`, `jsonlite`

---

## 📝 Usage

1. Run the GEE script (`s2_ndwi_ndci_processing.js`) to generate NDWI/NDCI raster outputs.
2. Place exported images into corresponding `data/NDCI_x/` folders.
3. Use R scripts in order:
   - Start with `ndci_sectional_analysis.R`
   - Then run `ndci_yearly_trend_analysis.R`
   - Finish with `phosphorus_overflow_correlation.R`
4. Outputs (plots and Excel summaries) will be saved in the `outputs/` folder.

---

## 📌 Notes

- File paths are relative—adjust them if running outside the cloned repo.
- Some scripts contain placeholders for loading input data (`fulldata_1`)—replace with your actual data as needed.
- This is an archived academic project, preserved for reference.

---

## 📄 License

MIT License – Free to use with attribution.
