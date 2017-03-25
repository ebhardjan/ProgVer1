#!/bin/bash
echo "3sat"
sbt "run-main util.PerformanceTestCNFGenerator [3,5,8,10,15,20,25,30,40,50,60,70,80,90,100,120] 3sat/ 20 3"

echo "5sat"
sbt "run-main util.PerformanceTestCNFGenerator [3,5,8,10,15,20,25,30,40,50] 5sat/ 20 5"
