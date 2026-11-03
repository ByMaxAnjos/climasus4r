# Step 2 of the Jacobson et al. (2021) benchmark: hourly ERA5-Land 2 m temperature
# and dewpoint at the centroid of each state capital, 2000-2017, from the
# Copernicus CDS point time-series dataset (needs ~/.cdsapirc and the ERA5-Land
# licence accepted). One zip per capital; existing files are skipped, so the
# script can be re-run to resume.
import csv, os, sys, time
from concurrent.futures import ThreadPoolExecutor
import cdsapi

here = os.path.dirname(os.path.abspath(__file__))
out_dir = os.path.join(here, "data", "era5_cds")
os.makedirs(out_dir, exist_ok=True)
caps = list(csv.DictReader(open(os.path.join(here, "data", "capital_centroids.csv"))))

def fetch(row):
    dest = os.path.join(out_dir, row["uf"] + ".zip")
    if os.path.exists(dest) and os.path.getsize(dest) > 0:
        return row["uf"], "cached"
    t = time.time()
    try:
        cdsapi.Client(quiet=True).retrieve("reanalysis-era5-land-timeseries", {
            "variable": ["2m_temperature", "2m_dewpoint_temperature"],
            "location": {"longitude": float(row["lon"]), "latitude": float(row["lat"])},
            "date": ["2000-01-01/2017-12-31"],
            "data_format": "csv",
        }, dest + ".part")
        os.replace(dest + ".part", dest)
        return row["uf"], "ok in %d s" % (time.time() - t)
    except Exception as e:  # keep going; failed capitals are re-run on the next call
        return row["uf"], "FAILED: %s" % str(e)[:200]

with ThreadPoolExecutor(max_workers=3) as ex:
    for uf, status in ex.map(fetch, caps):
        print(uf, status, flush=True)
