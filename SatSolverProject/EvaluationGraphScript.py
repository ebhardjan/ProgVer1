import matplotlib.pyplot as plt
import re

def convertToNumeral(inString):
    try:
        return float(inString)
    except:
        return None

inputFile = open('src/test/resources/solving/evaluations.txt')

# Extract information about running the test cases
line = inputFile.readline()
firstLinePattern = re.compile("nRuns=([0-9]*) i=(.*)")
firstLineMatch = firstLinePattern.match(line)

nRuns = firstLineMatch.group(1)
timeI = firstLineMatch.group(2)

# Extract runtimes into lists for each algorithm.
dpTimes = []
dpllTimes = []
cdclTimes = []

for line in inputFile:
    linePattern = re.compile(
        "dp:(([0-9]*.[0-9*])|x|i)  dpll:(([0-9]*.[0-9*])|x|i)  cdcl:(([0-9]*.[0-9*])|x|i)")
    lineMatch = linePattern.match(line)
    if lineMatch is None:
        continue
    dpTimes.append(lineMatch.group(1))
    dpllTimes.append(lineMatch.group(3))
    cdclTimes.append(lineMatch.group(5))


dpTimes = list(map(convertToNumeral, dpTimes))
dpllTimes = list(map(convertToNumeral, dpllTimes))
cdclTimes = list(map(convertToNumeral, cdclTimes))

testList = range(len(dpTimes))
fig, ax = plt.subplots()
ax.plot(testList, dpTimes, 'ko', label='DP', mfc='none')
ax.plot(testList, dpllTimes, 'bx', label='DPLL')
ax.plot(testList, cdclTimes, 'md', label='CDCL', mfc='none')

ax.set_title("Averaged over " + nRuns + " runs; timeout at " + timeI)

legend = ax.legend(loc='right')
plt.ylabel("Runtime [ms]")

# print(dpTimes)
# print(dpllTimes)
# print(cdclTimes)

plt.show()
