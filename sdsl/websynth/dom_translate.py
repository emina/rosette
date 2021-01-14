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
    f.write("#lang rosette\n\n")
    f.write('(require (only-in racket/runtime-path define-runtime-path))\n')
    f.write('(require "{wf}/dom.rkt")\n'.format(wf=args.websynth_filepath))
    f.write('(require "{wf}/websynth.rkt")\n'.format(wf=args.websynth_filepath))
    f.write('(require "{wf}/websynthlib.rkt")\n\n'.format(wf=args.websynth_filepath))
    
    # Define dom
    f.write('(define-runtime-path html (build-path "{wf}" "{html}"))\n'.format(wf=args.websynth_filepath,html=htmlpath))
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
            f.write('(define-symbolic {zpath_name} tag? #:length max_zpath_depth)\n'.format(zpath_name=zpath_name))
        f.write('\n')
        
        # Records Masks
        fieldmask_name = field_mask_name(r_index)
        f.write("(define-symbolic {field_mask_name} boolean? #:length max_zpath_depth)\n".format(field_mask_name=fieldmask_name))
    
    f.write('\n')
    f.write('; Cross-record Mask\n')
    f.write('(define-symbolic recordmask boolean? #:length max_zpath_depth)\n')
    
    f.write('(current-bitwidth #f)\n\n')

    f.write('(define (demonstration)\n\n');
    # All zpath asserts
    for r_index in range(0, rec_count):
        f.write('\t; Record {i} zpath asserts\n'.format(i=r_index))
        for f_index in range(0, field_count):
            zpath_name = field_zpath_name(r_index, f_index)
            f.write('\t(assert (path? {zpath_name} dom "{example}"))\n'.format(zpath_name=zpath_name,
                                                                             example=data[r_index][f_index]))
        f.write('\n')
    
    # Generate Masks
    if field_count >= 2 and rec_count >= 2:
        for r_index in range(rec_count):
            f.write('\t; Record {r} Field Mask Generation\n'.format(r=r_index))
            for f_index in range(field_count):
                if f_index == 0:
                    continue
                prev_zpath = field_zpath_name(r_index, f_index - 1)
                cur_zpath = field_zpath_name(r_index, f_index)
                field_mask = field_mask_name(r_index)
                f.write("\t(generate-mask {zpath_prev} {zpath_cur} {field_mask} max_zpath_depth)\n".format(zpath_prev=prev_zpath,
                                                                                                         zpath_cur=cur_zpath,
                                                                                                         field_mask=field_mask))
            f.write('\n')
        f.write('\n')
    # Record mask and Solve
    f.write('\t; Record Mask\n')
    f.write('\t(generate-mask r0f0zpath r1f0zpath recordmask max_zpath_depth))\n\n')

    f.write('; Solve\n')
    f.write('(define (scrape)\n')
    f.write('\t(define sol (solve (demonstration)))\n\n')
    
    # display all the zpaths!
    for r_index in range(rec_count):
        f.write('\t; Record {r} zpaths\n'.format(r=r_index))
        for f_index in range(field_count):
            # Record X zpaths
            zpath_name = field_zpath_name(r_index, f_index)
        
        # field mask for record
        fieldmask_name = field_mask_name(r_index)
    f.write('\n')
    
    # Make the zpaths
    f.write('\t; Construct final zpaths\n')
    for r_index in [0]:
        for f_index in range(field_count):
            zpath_name = field_zpath_name(r_index, f_index)
            f.write('\t(define {zpath_name}_list (map label (evaluate {zpath_name} sol)))\n'.format(zpath_name=zpath_name))
            f.write('\t(define generalized_{zpath_name}_list \n'.format(zpath_name=zpath_name))
            f.write('\t\t(apply-mask {zpath_name}_list (evaluate recordmask sol)))\n'.format(zpath_name=zpath_name))
            f.write('\t(define field{i}_zpath (synthsis_solution->zpath generalized_{zpath_name}_list))\n'.format(i=f_index,
                                                                                                                    zpath_name=zpath_name))
            f.write('\n')

    #if (args.rackunit_test == False) :
    #    f.write('\t(printf "DOM stats:  size = ~a, depth = ~a\\n" (size dom) max_zpath_depth)\n')
        
    # Scrape!
    f.write('\t(zip \n')
    for f_index in range(field_count):
        f.write('\t\t(DOM-Flatten (DOM-XPath dom field{i}_zpath))\n'.format(i=f_index))
    f.write('\t))\n\n')

    if (args.rackunit_test == False) :
        f.write('(scrape)\n')
    else :
        out = os.path.splitext(os.path.basename(args.html_filepath))[0]
        t = os.path.splitext(os.path.basename(args.examples))[0]
        f.write('(require rackunit rackunit/text-ui rosette/lib/roseunit)\n')
        f.write('(define-runtime-path out (build-path "." "{name}.out"))\n\n'.format(name=out))
        f.write('(define a-test\n')
        f.write('\t(test-suite+ \n')
        f.write('\t\t"{name}"\n'.format(name=t))
        f.write('\t\t(test-case "{name}"\n'.format(name=t))
        f.write('\t\t\t(define expected (second (call-with-input-file out read)))\n')
        f.write('\t\t\t(define actual (scrape))\n')
        f.write('\t\t\t(check-equal? actual expected))))\n')
        f.write('(time (run-tests a-test))\n')
        
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
    parser.add_argument("-test", "--rackunit_test", action="store_true", help="Generate Rackunit tests instead of standard output file")
    args = parser.parse_args()

    print("\n--OPTIONS--")
    print("Pretty Print: {}".format(args.pretty))    
    print("   Delimiter: {}".format(args.delimiter))
    print("Source HTML: {}".format(args.html_filepath))
    print("       Test: {}".format(args.rackunit_test))

    output = args.output_filepath
    for e in args.examples:
        args.examples = e
        args.output_filepath = '{out}/{file}.rkt'.format(out=output,file=os.path.splitext(os.path.basename(e))[0])
        print("\nExample File: {}".format(args.examples))
        print("Saving Racket/Rosette file: {}\n".format(args.output_filepath))
        generate(args)
    
