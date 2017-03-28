import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import re

inputFile = open('3sat/evaluations_sorted.txt')
AGGREGATE_N = 10
FILTER_STR = "_unsat"
VARIABLES_USED = [3,5,8,10,15,20,25,30,40,50,60,70,80,90,100,120]
# VARIABLES_USED = [3,5,8,10,15,20,25,30,40,50]

def convertToNumeral(inString):
    try:
        return float(inString)
    except:
        return None

def average(inList):
    av = 0
    for element in inList:
        k = convertToNumeral(element)
        if k != None:
            av += k
        else:
            return None
    return av / len(inList)

def aggregateResults(inList, n):
    result = []
    i = 0
    while (i < len(inList)):
        result.append(average(inList[i:i+n]))
        i += n
    return result

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
        "dp:(([0-9]*.[0-9*])|x|i)" +
        "  dpll:(([0-9]*.[0-9*])|x|i)" +
        "  cdcl:(([0-9]*.[0-9*])|x|i)" +
        " \(.*\)")
    lineMatch = linePattern.match(line)
    if lineMatch is None:
        continue
    if FILTER_STR in lineMatch.group(0):
        dpTimes.append(lineMatch.group(1))
        dpllTimes.append(lineMatch.group(3))
        cdclTimes.append(lineMatch.group(5))


dpTimes = list(map(convertToNumeral, dpTimes))
dpllTimes = list(map(convertToNumeral, dpllTimes))
cdclTimes = list(map(convertToNumeral, cdclTimes))

dpTimes = aggregateResults(dpTimes, AGGREGATE_N)
dpllTimes = aggregateResults(dpllTimes, AGGREGATE_N)
cdclTimes = aggregateResults(cdclTimes, AGGREGATE_N)

xAxis = range(len(dpTimes))
fig, ax = plt.subplots()
ax.semilogy(xAxis, dpTimes, 'ko-', label='DP', mfc='none')
ax.semilogy(xAxis, dpllTimes, 'bx-', label='DPLL')
ax.semilogy(xAxis, cdclTimes, 'md-', label='CDCL', mfc='none')

def xToNVar(x, pos):
    return VARIABLES_USED[int(x)]

fmt = ticker.FuncFormatter(xToNVar)
ax.xaxis.set_major_formatter(fmt)

titleText = "Timeout at " + timeI
if AGGREGATE_N > 1:
    titleText += "\n Averaged over " + str(AGGREGATE_N) + \
                 " formulas of same size"
ax.set_title(titleText)

legend = ax.legend(loc='right')
plt.ylabel("Runtime [ms]")
plt.xlabel("Number of variables")

# print(dpTimes)
# print(dpllTimes)
# print(cdclTimes)

plt.show()
