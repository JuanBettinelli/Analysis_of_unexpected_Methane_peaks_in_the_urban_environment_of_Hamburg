import os
import pandas as pd
import plotly.express as px
import plotly.offline as offline

# Path to the folder containing CSV files
folder_path = '/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Blankenese/'
# folder_path = '/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Bunthaus/'
# folder_path = '/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Seemanshöft/'
# folder_path = '/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_StPauli/'


# Initialize an empty list to store DataFrames
dfs = []

# Loop through each file in the folder
for filename in os.listdir(folder_path):
    if filename.endswith('.csv'):  # Check if the file is a CSV file
        # Read the CSV file into a DataFrame
        file_path = os.path.join(folder_path, filename)
        df = pd.read_csv(file_path, delimiter=';')
        
        # Convert 'Time' column to datetime format
        df.iloc[:, 0] = pd.to_datetime(df.iloc[:, 0], format='%H:%M', errors='coerce')
        
        # Replace '24:00' with '00:00' and add one day to the date
        df.loc[df.iloc[:, 0].dt.hour == 0, df.columns[0]] += pd.Timedelta(days=1)
        
        # Convert the date from the column name to datetime format
        date = pd.to_datetime(df.columns[0], format='%d.%m.%Y')
        
        # Create a new DataFrame with UTC timezone
        new_df = pd.DataFrame()
        new_df['UTC'] = pd.to_datetime(date.strftime('%Y-%m-%d') + ' ' + df.iloc[:, 0].dt.strftime('%H:%M'))
        #new_df['UTC'] = new_df['UTC'].dt.tz_localize('Europe/Paris').dt.tz_convert('UTC')
        new_df['WL_Height'] = df.iloc[:, 1]
        
        # Append the new DataFrame to the list
        dfs.append(new_df)

# Concatenate all DataFrames in the list into one DataFrame
merged_df = pd.concat(dfs, ignore_index=True)

# Sort the DataFrame by datetime
merged_df = merged_df.sort_values(by='UTC')

merged_df = merged_df.dropna()

merged_df.to_csv('/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Blankenese/WL_Blankenese_merged.csv', index=False)
# merged_df.to_csv('/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Bunthaus/WL_Bunthaus_merged.csv', index=False)
# merged_df.to_csv('/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Seemanshöft/WL_Seemanshöft_merged.csv', index=False)
# merged_df.to_csv('/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_StPauli/WL_StPauli_merged.csv', index=False)


fig = px.line(merged_df, x='UTC', y='WL_Height', title='Water level timeline')

# Customize x-axis label
fig.update_xaxes(title_text='UTC')

# Customize y-axis label
fig.update_yaxes(title_text='Water Level [cm]')

# Show the plot
fig.show()
offline.plot(fig, filename='/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Blankenese/WL_Blankenese.html', auto_open=False)
# offline.plot(fig, filename='/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Bunthaus/WL_Bunthaus.html', auto_open=False)
# offline.plot(fig, filename='/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_Seemanshöft/WL_Seemanshöft.html', auto_open=False)
# offline.plot(fig, filename='/Users/juanbettinelli/Desktop/Hamburg_2024_Data/Water_Level_StPauli/WL_StPauli.html', auto_open=False)
