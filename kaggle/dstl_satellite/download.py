import requests
import zipfile
import os

data_url = 'https://www.kaggle.com/c/dstl-satellite-imagery-feature-detection/download/'
data_path = "/Volumes/external/data/"
files = {
    "sample_submission.csv.zip" : "sample_submission.csv.zip",
    "grid_sizes.csv.zip" : "grid_sizes.csv.zip",
    #sixteen_band is 7 GB
    "sixteen_band.zip" : "sixteen_band.zip",
    #three_band is 13 GB
    "three_band.zip" : "three_band.zip",
    "train_geojson_v3.zip" : "train_geojson_v3.zip",
    "train_wkt_v4.csv.zip" : "train_wkt_v4.csv.zip"
    }

delete_folders = {"__MACOSX"}

# Kaggle Username and Password
kaggle_info = {'UserName': "jakewalker", 'Password': "kaggle_Do_nO_haRm_please"}

if not os.path.exists(data_path):
    os.makedirs(data_path)

for entry in files.keys():    
    #look the the existance of a file/folder with or without the .zip extension
    if (not os.path.exists((data_path + files[entry])[0:len(data_path + files[entry]) - 4])) and (not os.path.exists((data_path + files[entry]))):
        # Attempts to download the CSV file. Gets rejected if we are not logged in.
        request_url = data_url + entry
        r = requests.get(request_url)

        print("downloading " + entry + "...")
        # Login to Kaggle and retrieve the data.
        r = requests.post(r.url, data = kaggle_info)
        
        # Writes the data to a local file one chunk at a time.
        f = open(data_path + files[entry], 'w')
        for chunk in r.iter_content(chunk_size = 512 * 1024): # Reads 512KB at a time into memory
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
        f.close()
    else:
        print("already downloaded " + data_path + files[entry])

    if os.path.exists((data_path + files[entry])):
        #unzip file
        print("unzipping...")
        with zipfile.ZipFile(data_path + files[entry]) as myzip:
            myzip.extractall(path=data_path)
            
        #delte the zip file
        #print("deleting zip file...")
        #os.remove(data_path + files[entry])
    else:
        print("already unzipped")


for folder in delete_folders:
    print("removing " + folder + "...")
    try:
        os.rmdir(data_path + folder)
    except (OSError):
        print("failed to delete")
        
print("done")
