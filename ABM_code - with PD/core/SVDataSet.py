# A string value dataset

class SVDataSet:

    def __init__(self, out_file, separator, schedule):
        self.out_file = out_file
        self.separator = separator
        self.schedule = schedule
        self.data_sources = []
        self.data = dict()
        
    def record(self):
        # Get tick and record it
        if "tick" not in self.data:
            self.data["tick"] = []
        self.data["tick"].append(self.schedule.tick)
        
        # Record the value from each data source
        for data_source in self.data_sources:
            data_source_name = data_source.name
            if data_source_name not in self.data:
                self.data[data_source_name] = []
                
            self.data[data_source_name].append(data_source.getData())

    def addDataSource(self, data_source):
        self.data_sources.append(data_source)
        
    def write(self):
        with open(self.out_file, 'w') as out_file:
            print("Writing " + str(len(self.data) + 1) + " properties to " + self.out_file)
            # Write header
            header = "tick"
            for data_source in self.data_sources:
                header += self.separator
                header += data_source.name
            out_file.write(header)
            
            # Write data
            num_records = len(self.data["tick"])
            print("Writing " + str(num_records) + " rows")
            for record in range(0, num_records):
                row = "\n" + str(self.data["tick"][record])

                for data_source in self.data_sources:
                    row += self.separator
                    row += str(self.data[data_source.name][record])
                out_file.write(row)
                