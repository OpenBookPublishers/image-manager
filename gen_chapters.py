#!/usr/bin/env python3

import sys
import uuid

template = """
insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('{}', '{}', '{}', {});"""

fixed = [
    ('Introduction', '0', 800),
    ('Cover image', '', 900),
    ('Unassigned images', '', 1000)
    ]

def run():
    num_chapters = 5
    if len(sys.argv) == 2:
        num_chapters = int(sys.argv[1])
    for chapter in range(1, num_chapters + 1):
        u = str(uuid.uuid4())
        index = chapter * 10
        chapter_name = "Chapter {}".format(chapter)
        print(template.format(
            u, chapter_name, chapter, index
            ))
    for chapter_name, chapter, index in fixed:
        u = str(uuid.uuid4())
        print(template.format(
            u, chapter_name, chapter, index
            ))
        

        
if __name__ == '__main__':
    run()
