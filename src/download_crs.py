import io
import re
import zipfile
import pandas as pd
import requests

BULK_DOWNLOAD_URL = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile="
BASE_DATAFLOW = "https://sdmx.oecd.org/public/rest/dataflow/OECD.DCD.FSD/"
CRS_FLOW_URL = BASE_DATAFLOW + "DSD_CRS@DF_CRS/"

def get_full_crs_parquet_url(latest_flow: float = 1.3) -> str:
    """
        Fetch the latest CRS Parquet file ID from the OECD website. Necessary since the file ID changes with each update.
        
        Args:
            latest_flow (float): The latest flow version to fetch the CRS Parquet file ID.
        
        Returns:
            str: The full URL of the CRS Parquet file.
    """

    # Make a request to the OECD website 
    response = requests.get(f"{CRS_FLOW_URL}{latest_flow}")
    response.raise_for_status()
    content = response.text
    
    # Use regex to find the link to the CRS Parquet file in the HTML content (zip file starts with CRS-Parquet)
    search_string="CRS-Parquet"
    match = re.search(f"{re.escape(search_string)}(.*?)</", content)
    parquet_link = match.group(1).strip()
    file_id = parquet_link.split("=")[-1]

    # Construct the full URL for the CRS Parquet file
    file_url = BULK_DOWNLOAD_URL + file_id

    return file_url


def download_crs_parquet(file_url: str, save_to_path: str = None) -> pd.DataFrame:
    """
        Download the CRS Parquet file from the given URL and extract its contents.
        
        Args:
            file_url (str): The URL of the CRS Parquet file.
            save_to_path (str): The path to save the extracted files. If None, files are not saved.
        
        Returns:
            pd.DataFrame: A DataFrame containing the concatenated data from all parquet files.
    """

    # Make a request to download the zip file
    response = requests.get(file_url, stream=True)
    # Check if the request was successful
    response.raise_for_status()

    # Open the content as a zip file and extract the parquet files
    with zipfile.ZipFile(io.BytesIO(response.content)) as z:
        # Find all parquet files in the zip archive
        parquet_files = [name for name in z.namelist() if name.endswith(".parquet")]

        # If save_to_path is provided, save the files to the path
        if save_to_path:
            save_to_path.mkdir(parents=True, exist_ok=True)
            for file_name in parquet_files:
                with z.open(file_name) as f_in, (save_to_path / file_name).open(
                    "wb"
                ) as f_out:
                    f_out.write(f_in.read())
        
        files = [pd.read_parquet(z.open(file)) for file in parquet_files]

    if files:
        return pd.concat(files, ignore_index=True)
    else:
        raise ValueError("No parquet files found in the zip archive.")