from pathlib import Path
import subprocess
import re

SRC_FOLDER = "5sat"
TARGET_FILE = SRC_FOLDER + "/evaluations.txt"

def getFiles(folder):
    srcPath = Path(SRC_FOLDER)
    return [f for f in srcPath.iterdir() if (
        ".smt2" in str(f) or ".cnf" in str(f)
    )]


def evaluate(files):
    outFile = open(TARGET_FILE, 'w')
    dpPattern = re.compile("dp:(([0-9]*.[0-9*])|x|i)")
    dpllPattern = re.compile("dpll:(([0-9]*.[0-9*])|x|i)")
    cdclPattern = re.compile("cdcl:(([0-9]*.[0-9*])|x|i) \(.*\)")
    for i,file in enumerate(files):
        print("Evaluating file: " + str(file))
        try:
            dpResult = subprocess.check_output(
                'sbt "run /eval ' + str(file) + ' /dp"', shell=True).decode('utf-8')
            dpllResult = subprocess.check_output(
                'sbt "run /eval ' + str(file) + ' /dpll"', shell=True).decode('utf-8')
            cdclResult = subprocess.check_output(
                'sbt "run /eval ' + str(file) + ' /cdcl"', shell=True).decode('utf-8')
        except:
            outFile.write("Error evaluating " + str(file) + "\n")
            continue
        # Extract number of runs and max runtime on the first run.
        if (i == 0):
            niPattern = re.compile("nRuns=([0-9]*) i=(.*)")
            for line in dpResult.split('\n'):
                niMatch = niPattern.match(line)
                if niMatch:
                    outFile.write(niMatch.group(0) + '\n')
        # Extract the time measurements from all the prints the program made.
        finalLine = ""
        for line in dpResult.split('\n'):
            if dpPattern.match(line):
                finalLine += line + " "

        for line in dpllResult.split('\n'):
            if dpllPattern.match(line):
                finalLine += line + " "

        for line in cdclResult.split('\n'):
            if cdclPattern.match(line):
                finalLine += line + " "

        print("Result: " + finalLine)
        outFile.write(finalLine + '\n')

def main():
    subprocess.check_output('sbt compile', shell=True)
    evaluate(getFiles(SRC_FOLDER))


main()
