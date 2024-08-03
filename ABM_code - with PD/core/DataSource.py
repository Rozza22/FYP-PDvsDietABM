class DataSource:

    def __init__(self, statistics_collector, name):
        self.statistics_collector = statistics_collector
        self.name = name
        
    def getData(self):
        return self.statistics_collector.map[self.name]