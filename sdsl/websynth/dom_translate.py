#!/usr/bin/env python

from bs4 import BeautifulSoup
from bs4 import element
import sys
import argparse
import os

import efp

def _quote_escape(text):
    if isinstance(text, basestring):
        return text.replace(u'"', u'@QUOTE@')
    return unicode(text).replace(u'"', u'@QUOTE@')

def field_zpath_name(record_index, field_index):
    return 'r{r}f{f}zpath'.format(r=record_index, f=field_index)

def field_mask_name(record_index):
    return 'r{i}fieldmask'.format(i=record_index)

def generate(args):
    # print args.html_filepath, args.output_filepath
    htmlpath = args.html_filepath
    rktpath = args.output_filepath
    
    f = open(rktpath, 'w')
    
    # Racket headers.
    f.write("#lang s-exp rosette\n\n")
    f.write('(require (only-in racket/runtime-path define-runtime-path))\n')
    f.write('(require "{wf}/dom.rkt")\n'.format(wf=args.websynth_filepath))
    f.write('(require "{wf}/websynth.rkt")\n'.format(wf=args.websynth_filepath))
    f.write('(require "{wf}/websynthlib.rkt")\n\n'.format(wf=args.websynth_filepath))
    
    # Define dom
    f.write('(define-runtime-path html (build-path "." "{html}"))\n'.format(html=htmlpath))
    f.write('(define dom (read-DOMNode html))\n')
    # Define tags
    f.write('(define-tags (tags dom))\n')
       
    # Example Data
    data = efp.ef_parse(args.examples, args.delimiter)
    rec_count = efp.count_records(data)
    field_count = efp.count_fields(data)
    
    # Solver calls.
    f.write('(define max_zpath_depth (depth dom))\n\n')
    for r_index in range(0, rec_count):
        f.write('; Record {} fields\n'.format(r_index))
        
        # Create all the zpath symbolic variables
        for f_index in range(0, field_count):
            zpath_name = field_zpath_name(r_index, f_index)
            f.write('(define-symbolic {zpath_name} tag? [max_zpath_depth])\n'.format(zpath_name=zpath_name))
        f.write('\n')
        
        # Records Masks
        fieldmask_name = field_mask_name(r_index)
        f.write("(define-symbolic {field_mask_name} boolean? [max_zpath_depth])\n".format(field_mask_name=fieldmask_name))
    
    f.write('\n')
    f.write('; Cross-record Mask\n')
    f.write('(define-symbolic recordmask boolean? [max_zpath_depth])\n')
    
    
    f.write('(current-log-handler (log-handler #:info any/c))\n')
    f.write('(configure [bitwidth 1])\n\n')
    
    # All zpath asserts
    for r_index in range(0, rec_count):
        f.write('; Record {i} zpath asserts\n'.format(i=r_index))
        for f_index in range(0, field_count):
            zpath_name = field_zpath_name(r_index, f_index)
            f.write('(assert (path? {zpath_name} dom "{example}"))\n'.format(zpath_name=zpath_name,
                                                                             example=data[r_index][f_index]))
        f.write('\n')
    
    # Generate Masks
    if field_count >= 2 and rec_count >= 2:
        for r_index in range(rec_count):
            f.write('; Record {r} Field Mask Generation\n'.format(r=r_index))
            for f_index in range(field_count):
                if f_index == 0:
                    continue
                prev_zpath = field_zpath_name(r_index, f_index - 1)
                cur_zpath = field_zpath_name(r_index, f_index)
                field_mask = field_mask_name(r_index)
                f.write("(generate-mask {zpath_prev} {zpath_cur} {field_mask} max_zpath_depth)\n".format(zpath_prev=prev_zpath,
                                                                                                         zpath_cur=cur_zpath,
                                                                                                         field_mask=field_mask))
            f.write('\n')
        f.write('\n')
    # Record mask and Solve
    f.write('; Record Mask and Solve\n')
    f.write('(generate-mask r0f0zpath r1f0zpath recordmask max_zpath_depth)\n')
    f.write('(define sol (solve #t))\n\n')
    
    # display all the zpaths!
    for r_index in range(rec_count):
        f.write('; Record {r} zpaths\n'.format(r=r_index))
        for f_index in range(field_count):
            # Record X zpaths
            zpath_name = field_zpath_name(r_index, f_index)
        
        # field mask for record
        fieldmask_name = field_mask_name(r_index)
    f.write('\n')
    
    # Make the zpaths
    f.write('; Construct final zpaths\n')
    for r_index in [0]:
        for f_index in range(field_count):
            zpath_name = field_zpath_name(r_index, f_index)
            f.write('(define {zpath_name}_list (map label (evaluate {zpath_name})))\n'.format(zpath_name=zpath_name))
            f.write('(define generalizelized_{zpath_name}_list \n'.format(zpath_name=zpath_name))
            f.write('   (apply-mask {zpath_name}_list (evaluate recordmask)))\n'.format(zpath_name=zpath_name))
            f.write('(define field{i}_zpath (synthsis_solution->zpath generalizelized_{zpath_name}_list))\n'.format(i=f_index,
                                                                                                                    zpath_name=zpath_name))
            f.write('\n')
    
    f.write('(printf "DOM stats:  size = ~a, depth = ~a, tags = ~a\\n" (size dom) max_zpath_depth (enum-size tag?))\n')
    # Scrape!
    f.write('(zip \n')
    for f_index in range(field_count):
        f.write('(DOM-Flatten (DOM-XPath dom field{i}_zpath))\n'.format(i=f_index))
    f.write(')\n')
    
    f.close()
    print("{} converted and saved to {}.".format(htmlpath, rktpath))


if __name__ == '__main__':
    # if len(sys.argv) != 3:
    #    print("usage: {} <htmlfile> <dom_something.rkt>".format(sys.argv[0]))
    #    sys.exit(9)
    # htmlpath = sys.argv[1]
    # rktpath = sys.argv[2]

    parser = argparse.ArgumentParser()
    parser.add_argument("-html", "--html_filepath", help="Input HTML file")
    parser.add_argument("-output", "--output_filepath", help="Output file")
    parser.add_argument("-p", "--pretty", action="store_true", help="Pretty print the DOM during output")
    parser.add_argument("-f", "--examples", nargs='+', help="File containing example data to use in synthesis")
    parser.add_argument("-d", "--delimiter", default="\t", help="Delimiter to use for file provided with -f/--examples")
    parser.add_argument("-websynth", "--websynth_filepath", default=".", help="Location of the websynth implementation")
    args = parser.parse_args()

    print("\n--OPTIONS--")
    print("Pretty Print: {}".format(args.pretty))    
    print("   Delimiter: {}".format(args.delimiter))
    print("Source HTML: {}".format(args.html_filepath))

    output = args.output_filepath
    for e in args.examples:
        args.examples = e
        args.output_filepath = '{out}/{file}.rkt'.format(out=output,file=os.path.splitext(os.path.basename(e))[0])
        print("\nExample File: {}".format(args.examples))
        print("Saving Racket/Rosette file: {}\n".format(args.output_filepath))
        generate(args)
    