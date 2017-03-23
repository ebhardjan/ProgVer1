import matplotlib.pyplot as plt
import re

def convertToNumeral(inString):
    try:
        return float(inString)
    except:
        return None

inputFile = open('src/test/resources/solving/evaluations.txt')

# line = inputFile.readline()

# firstLinePattern = re.compile("nRuns=([0-9]*) i=(.*)")
# firstLineMatch = firstLinePattern.match(line)

# nRuns = firstLineMatch.group(1)
# timeI = firstLineMatch.group(2)

dpTimes = []
dpllTimes = []
cdclTimes = []

for line in inputFile:
    linePattern = re.compile(
        "dp:(([0-9]*.[0-9*])|x|i)  dpll:(([0-9]*.[0-9*])|x|i)  cdcl:(([0-9]*.[0-9*])|x|i)")
    lineMatch = linePattern.match(line)
    dpTimes.append(lineMatch.group(1))
    dpllTimes.append(lineMatch.group(3))
    cdclTimes.append(lineMatch.group(5))


dpTimes = list(map(convertToNumeral, dpTimes))
dpllTimes = list(map(convertToNumeral, dpllTimes))
cdclTimes = list(map(convertToNumeral, cdclTimes))

testList = range(len(dpTimes))
fig, ax = plt.subplots()
ax.plot(testList, dpTimes, 'k.', label='DP')
ax.plot(testList, dpllTimes, 'kx', label='DPLL')
ax.plot(testList, cdclTimes, 'k*', label='CDCL')

legend = ax.legend(loc='right')
plt.ylabel("Runtime [ms]")

# print(dpTimes)
# print(dpllTimes)
# print(cdclTimes)

plt.show()
