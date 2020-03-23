(ns help)

(defn help
  "Function to print help messages for a function"
  [function]
  (cond
    (or (nil? function) (= "idiot" function)) (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"<message>\" [(-p <parent>)...]\n   commit-tree <tree> -m \"<message>\" [(-p <parent>)...]\n   hash-object [-w] <file>\n   help\n   init\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
    (or (= "-h" function) (= "--help" function) (= "help" function))  (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   hash-object [-w] <file>\n   help\n   init\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
    (= "init" function) (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
    (= "hash-object" function) (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
    (= "cat-file" function) (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
    (= "write-wtree" function) (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
    (= "commit-tree" function) (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
    (= "rev-parse" function) (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
    (= "switch" function) (println "Usage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it\nOtherwise, if no arguments are given, print \"Error: you must specify a branch name.")
    (= "branch" function) (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
    :else (println "Error: invalid command")))