import csv

def maxBMIvalue(csv_filename):
    maxValue = None

    # Open the CSV file
    with open(csv_filename, 'r', newline='') as file:
        reader = csv.reader(file)
        headerLine = next(reader)  # Read the header line
        indexBmival = headerLine.index("bmival")  # Find the index of "bmival" column

        # Step 1: Find the max value in the "bmival" column
        for row in reader:
            value = float(row[indexBmival])
            if maxValue is None or value > maxValue:
                maxValue = value

    return maxValue

# Example usage:
csv_filename = "props/GLondon_basepop_1000.csv"  # Specify the path to your CSV file
# csv_filename = "props/GLondon_basepop_1000 - agent38.csv"
max_value = maxBMIvalue(csv_filename)
# print("Max BMI value:", max_value)