import os
import numpy as np
import pandas as pd 
import random 
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm

os.makedirs('results', exist_ok=True)

# Read processed CSV file 
df = pd.read_csv("ms_data.csv")

# Ensure data can be fully displayed in consule 
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)

# Initia State Analysis to view any corrections needed to Dtype
summarize_data = pd.DataFrame({
        'Column': df.columns,
        'Dtype': df.dtypes.values, 
    })
print(summarize_data)
print(df.shape)

# Convert to correct Dtype for correct data analysis 
df['visit_date'] = pd.to_datetime(df['visit_date'])
df['patient_id'] = df['patient_id'].astype('string')
df['education_level'] = df['education_level'].astype('category')

# Sort data by patient_id and visit_date to group patients chronologically for analysis 
df = df.sort_values(by = ['patient_id', 'visit_date'])

# Read Insurance Types in order to assign insurance types to patients
with open('insurance.lst', 'r') as f:
    insurance_type = [line.strip() for line in f]

# Adjust insurance cost based on insurance type to reflect insurance coverage 
INSURANCE_EFFECT = { 
    'Bronze' : -0.6,         # 60% cost reduction
    'Silver' : -0.65,        # 65% cost reduction
    'Gold': -0.8,            # 80% cost reduction
    'Platinum': -0.9         # 90% cost reduction
}

# Probability Weights for insurance type so probability of good insurance increase with education level
EDUCATION_WEIGHTS = { 
    'High School' : {
        'Bronze':0.5,
        'Silver':0.3,
        'Gold':0.15,
        'Platinum':0.05
    },

    'Some College': { 
    'Bronze': 0.35,
    'Silver' : 0.4,
    'Gold':0.20,
    'Platinum':0.05, 
    }, 

    'Bachelors': {
        'Bronze':0.15, 
        'Silver' : 0.25,
        'Gold' : 0.35,
        'Platinum' : 0.25
    }, 
     'Graduate': { 
         'Bronze': 0.1,
         'Silver': 0.2,
         'Gold' : 0.3,
         'Platinum': 0.40,
     }
}

# Get unique list of patient IDs to assign insurance once per patient
patients = list(df['patient_id'].unique())
patient_insurance = {}

# Assign insurance types to each patient based on their education level
for patient in patients:
    education = df[df['patient_id'] == patient]['education_level'].iloc[0]      
    weights = EDUCATION_WEIGHTS[education]                                      
    options = list(weights.keys())
    probabilities = list(weights.values())
    patient_insurance[patient] = np.random.choice(options, p = probabilities)   #insurance assignent still random but do have probabilities associated with outcome 

# Map each patient's insurance type to all their visists creating new column in df for insurance type 
df['insurance_type'] = df['patient_id'].map(patient_insurance)

# Set base cost for visit in order to calcualte costs based on insurance effect 
pre_cost = 1000

# Map each patient's visit cost to all their visits and calculate visit cost using reductions from insurance effect 
# Introduce uniform randomness so probability across price variation range is equal
df['visit_cost'] = pre_cost * np.random.uniform(0.8,1.2, size=len(df)) * (1 +df['insurance_type'].map(INSURANCE_EFFECT))


# Convert insurance_type to category since it has fixed values and for accurate data analysis 
df['insurance_type'] = df['insurance_type'].astype('category')

df.to_csv("ms_data_new.csv", index=False)

# Print final state analysis to ensure all Dtype accurate 
summarize_data = pd.DataFrame({
        'Column': df.columns,
        'Dtype': df.dtypes.values, 
    })
print(summarize_data)

#Calculate mean walking spead by education level 
mean_speed_education = df.groupby('education_level', observed=True)['walking_speed'].mean()

# Calculate mean costs by insurance type 
mean_costs_insurance = df.groupby('insurance_type', observed=True)['visit_cost'].mean()

# Conduct Linear Regerssion to find age effects on walking speed
X = sm.add_constant(df['age'])  # Add a constant term (for intercept)
y = df['walking_speed']
model = sm.OLS(y, X).fit()
age_coefficient = model.params['age']

#Write results into text file to organize results
with open("summary_statistics.txt", "w") as file:
    file.write("Summary Statistics\n")
    file.write("===================\n\n")
    file.write("Mean Walking Speed by Education Level:\n")
    file.write(mean_speed_education.to_string()) 
    file.write("\n\n")
    file.write("Mean Costs by Insurance Type:\n")
    file.write(mean_costs_insurance.to_string())
    file.write("\n\n")
    file.write(f"Walking speed decreases by {age_coefficient:.4f} feet/second per year.")

