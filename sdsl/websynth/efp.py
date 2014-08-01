'''
This is the Example Field Parser (EFP), which takes in the user input and turns it into usable data for Websynth.
'''

def ef_parse(filename, delimiter):
    with open(filename, 'r') as f:
        data = []
        for line in f:
            if not line[0] == '#':
                line = line.split(delimiter)
                line = [ token.strip() for token in line ]
                data.append(line)
    return data

def count_records(data):
    return len(data)

def count_fields(data):
    return len(data[0])
