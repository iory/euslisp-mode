#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import re
import roslib


def main():
    import sys
    if len(sys.argv[1:]) < 1:
        sys.exit(1)
    inDirs = []
    inDirs.extend([roslib.packages.get_pkg_dir('euslisp'),
                   roslib.packages.get_pkg_dir('roseus'),
                   ])
    inDirs.extend(sys.argv[1:])
    # print(inDirs)

    pattern = sys.argv[1]
    patterns = [
        '\(:{0}'.format(pattern),
        '\(defun {0}'.format(pattern),
        '\(defclass {0}'.format(pattern),
        '\(defmethods {0}'.format(pattern),
        '\(defmacro {0}'.format(pattern),
    ]
    matches = [re.compile(p) for p in patterns]

    fileList = []
    for inDir in inDirs:
        for dName, sdName, fList in os.walk(inDir):
            for filename in fList:
                if not filename.endswith('.l'):
                    continue
                if not os.path.exists(os.path.join(dName, filename)):
                    continue
                with open(os.path.join(dName, filename), 'r') as f:
                    for c, line in enumerate(f.readlines()):
                        for match in matches:
                            if match.search(line):
                                fileList.append(
                                    "***".join([os.path.join(dName, filename), str(c), line[:-1].lstrip()]))
                                break

    class_path = os.path.join(roslib.packages.get_pkg_dir(
        'euslisp'), "jskeus/eus/doc/classes")
    if os.path.exists(class_path):
        with open(class_path, 'r') as f:
            for c, line in enumerate(f.readlines()):
                for match in matches:
                    if match.search(line):
                        fileList.append(
                            "***".join([class_path, str(c), line[:-1].lstrip()]))
                        break
    fileList = list(set(fileList))
    print("****".join(fileList))


if __name__ == "__main__":
    main()
