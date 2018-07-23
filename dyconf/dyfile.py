
# ---------------------------------------------------------------------
# FILE DIRECTORIES

"""
Where to find the source files.
"""
SRC_DIR					= "src"

"""
Where to store the documentation files.
"""
DEST_DIR				= "docs"

# ---------------------------------------------------------------------
# PROJECT DESCRIPTION

"""
Give your project a name.
"""
PROJECT_NAME			= "Sycalc"

"""
Add a description for your project. It may contain html code.
"""
PROJECT_DESCRIPTION		= "Sycalc is a small symbolic calculator \
written in Prolog. It offers the basic arithmetic operations between \
polynomials (except for the quotient)"

# ---------------------------------------------------------------------
# GENERATED GRAPHS

"""
For every file, make a directed graph showing what files
include (load) what files. In this graph a directed
edge (A,B) means 'file A includes (loads) file B'.
Allowed values: True, False
"""
FILE_INCLUSION_GRAPH	= False

"""
Same as for FILE_INCLUSION_GRAPH but this graph contains
the graph for all the source files.
Allowed values: True, False
"""
PROJECT_INCLUSION_GRAPH	= False

"""
Remove or keep the .dot files used to generate the inclusion graphs.
Allowed values: True, False
"""
KEEP_DOT				= False

"""
The maximum distance between a source vertex and a sink vertex
in the file inclusion graph is at most the value. A value less than
or equal to 0 is interpreted as infinite.
Allowed values: any numerical value.
"""
FILE_GRAPH_MAX_DIAMETER		= 0
PROJECT_GRAPH_MAX_DIAMETER	= 0

# ---------------------------------------------------------------------
# HOW TO TRAVERSE THE DIRECTORIES AND WHAT FILES SHOULD BE PARSED

"""
Document all files found in the source directories
and the directories within it.
Allowed values: True, False
"""
RECURSIVE				= True

"""
Document all files whose extension match the ones
in the list.
Allowed values: any list of strings of alphanumeric characters, all
of them starting with a '.'.
"""
EXTENSIONS				= [".pl", ".prolog"]

"""
Document files included (loaded) by other files even
if they are in a folder different from the SRC_DIR and
the RECURSIVE variable is set to False.
Allowed values: True, False
"""
FOLLOW_INCLUDES			= True

# ---------------------------------------------------------------------
# PATH TO DOT EXECUTABLE

"""
Modify this variable appropriately so that python can
find the program to generate the graphs (dot, part of graphViz).
Allowed values: a single string of alphanumeric characters that
represent a path to the executable used to generate the graphs.
This executable must understand the DOT language. For more information,
see: https://www.graphviz.org/
"""
DOT_EXE_PATH			= "/usr/bin/"
