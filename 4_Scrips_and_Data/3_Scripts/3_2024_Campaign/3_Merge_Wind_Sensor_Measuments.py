
import os
import pandas as pd
import plotly.express as px
import plotly.offline as offline
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from windrose import WindroseAxes
from matplotlib import pyplot as plt
import matplotlib.cm as cm
import numpy as np

# Path to the folder containing the CSV files
folder_path = '/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data/4_Data/10_2024_Data/Windsensor'


# Initialize an empty list to store DataFrames
dfs = []

# Loop through each file in the folder
for filename in os.listdir(folder_path):
    if filename.endswith('.xlsx'):  # Check if the file is a CSV file
        # Read the CSV file into a DataFrame
        file_path = os.path.join(folder_path, filename)
        df = pd.read_excel(file_path)
        
        new_column_names = {'Unnamed: 0': 'UTC', '061.1119.0811.230 61': 'Sensor_Wind_Speed', 'Unnamed: 2': 'Sensor_Wind_Direction'}
        # Use the rename() method to rename columns
        df = df.rename(columns=new_column_names)
        df = df.drop(df.index[:2])
        df['UTC'] = pd.to_datetime(df['UTC'])
        # Assuming 'column1' and 'column2' are the names of the columns
        # Replace 'column1' and 'column2' with the actual names of your columns
        df['Sensor_Wind_Speed'] = pd.to_numeric(df['Sensor_Wind_Speed'], errors='coerce')
        df['Sensor_Wind_Direction'] = pd.to_numeric(df['Sensor_Wind_Direction'], errors='coerce')

        # Remove rows with NaN values in both columns
        df = df.dropna(subset=['Sensor_Wind_Speed', 'Sensor_Wind_Direction'])

        
        # Append the new DataFrame to the list
        dfs.append(df)

# Concatenate all DataFrames in the list into one DataFrame
merged_df = pd.concat(dfs, ignore_index=True)


# Sort the DataFrame by datetime
merged_df = merged_df.sort_values(by='UTC')

merged_df = merged_df.drop_duplicates()

merged_df = merged_df.dropna()

# export the merged DataFrame to a CSV file
merged_df.to_csv('/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data/4_Data/10_2024_Data/Windsensor/Merged_Wind_Measuments.csv', index=False)


