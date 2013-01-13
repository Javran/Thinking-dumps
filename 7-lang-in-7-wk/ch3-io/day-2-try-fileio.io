#!/usr/bin/env io

# this io source code tests file I/O methods 
#     for exercise #7 at day 2

# please make sure not to put any important things
#     in file 'day-2-try-fileio-test.txt',
#     which might be overwritten and removed 
#     during the test

f := File with("day-2-try-fileio-test.txt")
f remove
f openForUpdating

"Opening a file and write numbers into it..." println

originData := list(1, 3, 5, 7, 9, 8, 6, 4, 2, 0)
originData foreach(num, f write(num asString, "\n"))
f close

"File written." println

"Reading from the file..." println
f openForReading
data := f readLines map(item, item asNumber)

"Outputing content:" println
data println

"Verifying data ... " print
if ((data == originData), "ok", "failed") println

"Removing file ... " println
f remove

"Test done." println
