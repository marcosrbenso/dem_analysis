## 📁 Dataset Description: Digital Elevation Models (DEMs)

This dataset includes various Digital Elevation Models (DEMs) used for accuracy assessment against a reference topographic model. The data was prepared for evaluating vertical error across different acquisition methods and processing strategies.

---

### 📄 Raster Files

| File Name                  | Description                                                                 |
|---------------------------|-----------------------------------------------------------------------------|
| `SRTM.tif`                | Shuttle Radar Topography Mission (SRTM) DEM with ~1m resolution (resampled) |
| `ANADEM.tif`              | Brazilian National DEM (ANADEM), processed and reprojected for comparison   |
| `PHANTOM VOO 250M.tif`    | DEM captured by a Phantom drone flight covering a ~250m area                |
| `TOPOGRAFIA.tif`          | Ground truth topography — high-accuracy reference model (e.g., GNSS, total station) |
| `MAVIK.tif`               | DEM from RTK-equipped Mavic drone for higher positional precision           |
| `PHANTOM SEM PC.tif`      | DEM from Phantom drone **without ground control points (GCPs)**             |
| `PHANTOM COM PC.tif`      | DEM from Phantom drone **with GCPs**, expected to yield improved accuracy   |

---

### 📐 Spatial Information

- All rasters are projected to **EPSG:31982** (SIRGAS 2000 / UTM zone 22S).
- Resolution varies by source (SRTM is resampled, drone data ~1m).
- Coverage: small watershed or test area for DEM accuracy evaluation.

---

### 📚 Usage Notes

These layers are intended for use in:

- Vertical accuracy evaluation (e.g., RMSE, MAE, PBIAS)
- Visual inspection of elevation errors
- Training and benchmarking of DEM correction algorithms

Make sure to align all DEMs to the same CRS and resolution before comparative analysis.

---

### 🧾 Citation

If you use this dataset, please cite the accompanying GitHub repository and/or reference material:

> Benso, M. R. (2025). *DEM Accuracy Analysis Toolkit* [Computer software]. GitHub. https://github.com/marcosrbenso/dem_analysis
