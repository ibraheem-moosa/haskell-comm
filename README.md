# haskell-comm
A haskell implementation of unix comm which compares two sorted files and prints the unique and common lines.

comm is a unix command that takes as input two sorted files and prints the lines that are unique to first file,
unique to second file and common between the two files in three columns. We can omit the printing of a particular
column by using command-line arguments. "-1", "-2" and "-3" ommits first, second and third column respectively.
This arguments can be used combinedly too for example "-12", "-13" etc. comm also supports checking if the files
are actually sorted and the sorting order can controlled by environment variables such as LC_ALL, LC_COLLATE etc.
Also we can set custom delimiter between columns.

This is a partial implementation of comm in haskell. It only supports column ommiting funtionality right now. We will
add custom delimiter support next and then sorted order checking of files after that.
