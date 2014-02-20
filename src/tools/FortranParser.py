"""Script for parsing SLICOT fortran files, and generating
raw function headers. Super sloppy, unpythonic code right now.
But it works."""

import textwrap
import glob
import os
import pprint

def get_header(text):
    """Returns name, params for a Fortran Subroutine"""
    header = ''
    for l in text.splitlines():
        if header:
            if ')' in l:
                i = l.find(')')
                header += l[:i+1]
                break
            else:
                header += l
        if 'SUBROUTINE' in l:
            header = l.strip()
    data = [d.strip() for d in header.split('$')]
    header = ' '.join(data)
    paren1 = header.find('(')
    paren2 = header.find(')')
    name = header[11: paren1]
    params = header[paren1+1:paren2].strip().split(', ')
    params = [p.strip() for p in params if p]
    return name, params

def get_typedict(text):
    """Returns a dict of param: type for a Fortran Subroutine"""
    types = ['CHARACTER*', 'CHARACTER', 'DOUBLE PRECISION', 'INTEGER', 'LOGICAL', 'COMPLEX*16', 'EXTERNAL']
    typedict = {}
    hastype = False
    type_this_line = False
    for l in text.splitlines():
        if l.startswith('C'):
            continue
        elif hastype and '$' in l:
            plist = [p.strip().strip(',') for p in l[l.find('$')+1:].split(' ') if p]
            pdict = parse_params(ctype, plist)
            typedict.update(**pdict)
            continue
        for t in types:
            if t in l:
                hastype = True
                type_this_line = True
                ctype = t            
                plist = [p.strip().strip(',') for p in l[l.find(t)+len(t):].split(' ') if p]
                if ctype == "CHARACTER*":
                    ctype += plist.pop(0)
                pdict = parse_params(ctype, plist)
                typedict.update(**pdict)
                break
        if not type_this_line:
            hastype = False
        type_this_line = False
    return typedict

def parse_params(ctype, plist):
    """Parses a list of params into a dict of param: type"""
    pdict = {}
    for p in plist:
        if '(' in p:
            #It's an array
            ndims = p.count(',') + 1
            name = p[:p.find('(')]
            val = (ctype, ndims)
        else:
            name = p
            val = (ctype,)
        pdict[name] = val
    return pdict

class Formatter:
    def __init__(self, typedict):
        self.typedict = typedict
        self.headlookup={'CHARACTER': 'Char',
                        'INTEGER': 'Integer',
                        'DOUBLE PRECISION': 'FloatingPoint',
                        'COMPLEX*16': 'Complex',
                        'LOGICAL': 'Bool'}
        self.calllookup={'CHARACTER': 'Char',
                        'INTEGER': 'BlasInt',
                        'DOUBLE PRECISION': 'Float64',
                        'COMPLEX*16': 'Complex128',
                        'LOGICAL': 'Bool'}
        self.funcformat = """{:}

    INFO = [0]

    {:}
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in {:}: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end"""

    def header(self, funcname, params):
        #Create the header
        header = "function {:}!(".format(funcname.lower())
        call_list = [self._header(p) for p in params if p != "INFO"]
        if None in call_list:
            return None
        header += ', '.join(call_list) + ')'
        return header

    def callsig(self, funcname, params):
        #Get Call Signature
        callval_list = []
        callsig_list = []
        for p in params:
            callsig, callval = self._call(p)
            if callsig is None:
                return None
            callsig_list.append(callsig)
            callval_list.append(callval)
        sigstring = ', '.join(callsig_list)
        valstring = ', '.join(callval_list)
        callsig = 'ccall((:{:}_, "libslicot"), Void, ({:}), {:})'.format(funcname.lower(), sigstring, valstring)
        return callsig

    def subroutine(self, funcname, params):
        header = self.header(funcname, params)
        callsig = self.callsig(funcname, params)
        if header is None or callsig is None:
            return None
        headerf = textwrap.fill(header, subsequent_indent=' '*4)
        callsigf = textwrap.fill(callsig, width=68, subsequent_indent=' '*12)
        return self.funcformat.format(headerf, callsigf, funcname)

    def _header(self, name):
        args = self.typedict[name]
        if 'EXTERNAL' in args:
            return None
        if len(args) == 2:
            #It's an array
            ctype, ndims = args
            jtype = self.headlookup[ctype]
            return "{:}::Array{{{:},{:}}}".format(name,jtype,ndims)
        else:
            return "{:}::{:}".format(name,self.headlookup[args[0]])

    def _call(self, name):
        args = self.typedict[name]
        if 'EXTERNAL' in args:
            return (None, None)
        if len(args) == 2:
            call_val = name
        else:
            call_val = '&{:}'.format(name)
        jtype = self.calllookup[args[0]]
        call_sig = 'Ptr{{{:}}}'.format(jtype)
        return (call_sig, call_val)


if __name__ == '__main__':
    fils = glob.glob('../slicot/src/[A-Z]*.f')
    fils.sort()
    #fils = ['../slicot/src/AB08MZ.f',]
    for fil in fils:
        if 'BB0' in fil or 'BD0' in fil:
            #It's a benchmark, skip it
            continue
        if 'UD0' in fil or 'UE0' in fil:
            #It's a helper func, skip it
            continue
        #Get the text
        text = open(fil).read()

        funcname, funcparams = get_header(text)
        if not funcname:
            #It's a function, not a subroutine. Continue, will do later
            continue
        typedict = get_typedict(text)
        typedict["INFO"] = ("INTEGER", 1)
        formatter = Formatter(typedict)
        function = formatter.subroutine(funcname, funcparams)
        if function:
            #function is none if requires EXTERNAL
            print function
            print "\n"
