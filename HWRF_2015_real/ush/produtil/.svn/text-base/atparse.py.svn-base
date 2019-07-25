import sys, os, re, StringIO, logging

functions=dict(lc=lambda x:str(x).lower(),
               uc=lambda x:str(x).upper(),
               len=lambda x:str(len(x)),
               trim=lambda x:str(x).strip())

class ParserSyntaxError(Exception): 
    """Raised when the parser encounters a syntax error."""
class ScriptAssertion(Exception):
    """Raised when a script @[VARNAME:?message] is encountered, and
    the variable does not exist."""
class ScriptAbort(Exception): 
    """Raised when an "@** abort" directive is reached in a script."""
class NoSuchVariable(Exception):
    """Raised when a script requests an unknown variable."""
    def __init__(self,infile,varname,line=None):
        self.infile=infile
        self.varname=varname
        if line is None:
            self.line=None
            line='??'
        else:
            self.line=int(line)
            line=str(line)
        super(NoSuchVariable,self).__init__(
            '%s:%s: undefined variable %s'%(infile,line,varname))

def replace_backslashed(text):
    """Turns \t to tab, \n to end of line, \r to carrage return, \b to
    backspace and \(octal) to other characters."""
    if '0123456789'.find(text[1])>=0:
        return chr(int(text[1:],8))
    elif text=='\\n':
        return "\n"
    elif text=='\\t':
        return "\t"
    elif text=='\\r':
        return "\r"
    elif text=='\\b':
        return "\b"
    else:
        return text

# Parser states:
outer=dict(active=True,in_if_block=False,in_ifelse_block=False,used_if=False,ignore=False)
if_unused_if=dict(active=False,in_if_block=True,in_ifelse_block=False,used_if=False,ignore=False)
if_active_if=dict(active=True,in_if_block=True,in_ifelse_block=False,used_if=True,ignore=False)
if_used_if=dict(active=False,in_if_block=True,in_ifelse_block=True,used_if=True,ignore=False)
if_active_else=dict(active=True,in_if_block=False,in_ifelse_block=True,used_if=True,ignore=False)
if_inactive_else=dict(active=False,in_if_block=False,in_ifelse_block=True,used_if=True,ignore=False)

ignore_if_block=dict(active=False,in_if_block=True,in_ifelse_block=False,used_if=False,ignore=True)
ignore_else_block=dict(active=False,in_if_block=False,in_ifelse_block=True,used_if=False,ignore=True)

class ATParser:
    def __init__(self,stream=sys.stdout,varhash=None,logger=None,
                 max_lines=1000000):
        if varhash is None:
            self.varhash=dict(os.environ)
        else:
            self.varhash=dict(varhash)
        self.__infiles=['(string)']
        self._states=list()
        self.__stream=stream
        self.__max_lines=int(max_lines)
        self.__logger=logger
    def warn(self,text):
        if self.__logger is not None:
            self.__logger.warn(text)
    @property
    def max_lines(self):
        return self.__max_lines
    @property
    def infile(self):
        return self.__infiles[-1]
    def _write(self,data):
        self.__stream.write(data)
    def applyfun(self,val,fun1,morefun):
        runme=functions.get(fun1,None)
        if runme is not None:
            val=runme(val)
            if val is None: val=''
        else:
            self.warn(
                'Ignoring unknown function \"%s\" -- I only know these: %s'
                 %(fun1, ' '.join(functions.keys())))
        m=re.match('\.([A-Za-z0-9_]+)(.*)',morefun)
        if m:
            (fun2,morefun2)=m.groups()
            return self.applyfun(val,fun2,morefun2)
        return val

    def from_var(self,varname,optional):
        m=re.match('([A-Za-z0-9_]+)\.([A-Za-z0-9_]+)(.*)',varname)
        if m:
            (varname,fun1,morefun)=m.groups()
            val=self.from_var(varname,optional=optional)
            return self.applyfun(val,fun1,morefun)
        elif varname in self.varhash:
            return self.varhash[varname]
        elif optional:
            return ''
        else:
            raise NoSuchVariable(self.infile,varname)
    
    def optional_var(self,varname):
        return self.from_var(varname,optional=True)

    def require_var(self,varname):
        return self.from_var(varname,optional=False)
    
    def replace_vars(self,text):
        (text,n) = re.subn(r'(?<!\\)\$[a-zA-Z_0-9.]+',
                             lambda x: self.require_var(x.group(0)[1:]),
                             text)
        (text,n) = re.subn(r'(?<!\\)\$\{[^{}]*\}',
                             lambda x: self.var_or_command(x.group(0)[2:-1]),
                             text)
        (text,n) = re.subn(r'\\([0-9]{3}|.)',
                             lambda x: replace_backslashed(x.group(0)),text)
        return text
    def parse_stream(self,stream,streamname):
        lineno=1
        for line in stream:
            self.parse_line(line,streamname,lineno)
            lineno+=1

    def parse_file(self,filename):
        lineno=1
        with open(filename,'rt') as f:
            for line in f:
                self.parse_line(line,filename,lineno)
                lineno+=1

    def require_file(self,filename_pattern):
        filename=self.replace_vars(filename_pattern)
        with open(filename,'rt') as f:
            return f.read()

    def getvar(self,varname):
        if varname in self.varhash: return self.varhash[varname]
        return None
    def var_or_command(self,data):
        m=re.match(r'(?ms)\A([a-z_A-Z][a-zA-Z_0-9]*)'
                   r'((?:\.[A-Za-z0-9.]+)?)'
                   r'(?:(==|!=|:\+|:-|=|:=|:\?|<|:<|:)(.*))?\Z',
                   data)
        if not m:
            return ''
        (varname,functions,operator,operand)=m.groups()
        if operator:
            if operand is None: operand=''
            vartext=self.getvar(varname)
            varset = vartext is not None and vartext!=''
            if functions:
                if vartext is None: varetext=''
                mf=re.match(r'\A\.([A-Z0-9a-z_]+)(.*)\Z',functions)
                (fun,morefun)=mf.groups()
                vartext=self.applyfun(vartext,fun,morefun)
            if operator==':+':
                return self.replace_vars(operand) if varset else ''
            elif operator==':-':
                if not varset: vartext=self.replace_vars(operand)
                return vartext
            elif operator==':':
                val=vartext
                if val is None: val=''
                mo=re.match(r'\A([0-9]+)(?::([0-9]+))?',operand)
                if mo is None:
                    return val
                (start,count)=mo.groups()
                length=len(val)
                if start is None or start=='':
                    start=0
                else:
                    start=int(start)
                if start<0:
                    start=0
                if count is None or count=='':
                    count=length-start
                else:
                    count=int(count)
                if start+count>length:
                    count=length-start
                return val[ start : (start+count) ]
            elif operator=='=':
                replaced=self.replace_vars(operand)
                self.varhash[varname]=replaced
            elif operator=='==' or operator=='!=':
                # This is the ternary ?: operator.
                val=vartext
                mo=re.match(r'(?ms)\A((?:[^\\\?]|(?:\\\\)*|(?:\\\\)*\\.)*)\?(.*?):((?:[^\\:]|(?:\\\\)*|(?:\\\\)*\\.)*)\Z',operand)
                if mo is None:
                    (test,thendo,elsedo)=('','','')
                else:
                    (test,thendo,elsedo)=mo.groups()
                test=self.replace_vars(test)
                if operator=='==':
                    return self.replace_vars( 
                        thendo if (val==test) else elsedo)
                else:
                    return self.replace_vars(
                        thendo if (val!=test) else elsedo)
            elif operator==':=':
                if not varset:
                    self.varhash[varname]=self.replace_vars(operand)
                return self.varhash[varname]
            elif operator==':?':
                if varset:
                    return vartext
                elif operand=='':
                    raise ScriptAssertion('%s: you did not define this '
                                          'variable.  Aborting.'%(varname,))
                else:
                    raise ScriptAssertion('%s: %s'%(varname,operand))
        elif varname is not None and varname!='':
            return self.require_var(varname+functions)
        else:
            raise ParserSyntaxError(
                "Don't know what to do with text \"%s\""%(data,))

    def require_data(self,data):
        if data[0]=='<':
            # This is an instruction to read in a file.
            return self.require_file(data[1:])
        elif data=='@':
            return '@' # @[@] is replaced with @
        elif data[0]=='#':
            if data.find('@[')>=0:
                raise ParserSyntaxError('Found a @[ construct nested within a comment (@[#...])')
            return '' # @[#stuff] is a comment
        else:
            # This is a variable name, command or error:
            return self.var_or_command(data)

    def str_state(self):
        out=StringIO.StringIO()
        out.write('STATE STACK: \n')
        for state in self._states:
            out.write('state: ')
            if state['ignore']: 
                out.write('ignoring block: ')
            out.write('active ' if(state['active']) else 'inactive ')
            if state['in_if_block']: 
                out.write('in if block, before else ')
            if state['in_ifelse_block']: 
                out.write('in if block, after else ')
            if not state['in_if_block'] and not state['in_ifelse_block']:
                out.write('not if or else')
            if state['used_if']:
                out.write('(have activated a past if/elseif/else) ')
            out.write('\n')
        out.write('END\n')
        s=out.getvalue()
        out.close()
        return s
    
    @property
    def active(self):
        if self._states:
            for state in self._states:
                if not state['active']:
                    return False
        return True

    def top_state(self,what=None):
        if what:
            if not self._states:
                raise AssertionError('Internal error: no state to search when looking for %s in top state.'%(what,))
            elif what not in self._states[-1]:
                raise AssertionError('Internal error: cannot find %s in top state.'%(what,))
            return bool(self._states[-1][what])
        else:
            return self._states[-1]

    def push_state(self,state):
        self._states.append(state)

    def pop_state(self):
        return self._states.pop()

    def replace_state(self,state):
        self._states[len(self._states)-1]=state

    def parse_lines(self,lines,filename):
        lineno=1
        for line in lines.splitlines():
            self.parse_line(line,filename,lineno)
            lineno+=1

    def parse_line(self,line,filename,lineno):
        top_state=self.top_state
        replace_state=self.replace_state

        m=re.match(r'^\s*\@\*\*\s*if\s+([A-Za-z_][A-Za-z_0-9.]*)\s*([!=])=\s*(.*?)\s*$',line)
        if m:
            # This is the beginning of an IF block
            if not self.active:
                # This IF lies within an inactive block, so we skip
                # this whole if, elseif, else, endif block.
                self.push_state(ignore_if_block)
                return
            (left,comp,right)=m.groups()
            left=self.optional_var(left)
            right=self.replace_vars(right)
            if left==right:
                if comp=='=':
                    self.push_state(if_active_if)
                else:
                    self.push_state(if_unused_if)
#                self.push_state( if_active_if if(comp=='=') else if_unused_if )
            else:
                if comp=='=':
                    self.push_state(if_unused_if)
                else:
                    self.push_state(if_active_if)
#                self.push_state( if_unused_if if(comp=='=') else if_active_if )
            return

        m=re.match(r'^\s*\@\*\*\s*abort\s+(.*)$',line)
        if m:
            if self.active:
                raise ScriptAbort('Found an abort directive on line %d: %s'%(
                    lineno, m.group(1)))
            return

        m=re.match(r'^\s*\@\*\*\s*warn\s+(.*)$',line)
        if m:
            if self.active:
                self.warn(self.replace_vars(m.group(1)))
            return

        m=re.match('^\s*\@\*\*\s*else\s*if\s+([A-Za-z_][A-Za-z_0-9.]*)\s*([!=])=\s*(.*?)\s*\Z',line)
        if m:
            if top_state('ignore'): return
            (left, comp, right) = m.groups()
            left=self.optional_var(left)
            right=self.replace_vars(right)
            if not self._states:
                raise ParserSyntaxError(
                    'Found an elseif without a matching if at line %d'%lineno)
            if not top_state('in_if_block'):
                if top_state('in_ifelse_block'):
                    raise ParserSyntaxError(
                        'Unexpected elseif after an else at line %d'%lineno)
                else:
                    raise ParserSyntaxError(
                        'Unexpected elseif at line %d'%lineno)
            elif top_state('used_if'):
                # the "if" or a prior elseif matched, so we ignore
                # this elseif and deactivate the block so all future
                # if/else/elseif will be unused.
                replace_state(if_used_if)
            elif not top_state('active'):
                activate=0
                if left==right:
                    activate = 3 if (comp=='=') else 0
                else:
                    activate = 0 if (comp=='=') else 3
                if activate:
                    replace_state(if_active_if)
            return

        m=re.match(r'^\s*\@\*\*\s*else\s*(?:\#.*)?$',line)
        if m:
            if top_state("used_if"):
                replace_state(if_inactive_else)
            elif top_state('in_ifelse_block'):
                raise ParserSyntaxError('Found an extra else at line %d'%lineno)
            elif not top_state('in_if_block'):
                raise ParserSyntaxError('Found an else outside an if at line %d'%lineno)
            elif top_state('ignore'):
                # We're ignoring a whole if/elseif/else/endif block
                # because it lies within an inactive block.
                replace_state(ignore_else_block)
            elif not top_state('used_if'):
                replace_state(if_active_else)
            else:
                replace_state(if_inactive_else)
            return

        m=re.match(r'^\s*\@\*\*\s*endif\s*(?:\#.*)?$',line)
        if m:
            if top_state('in_if_block') or top_state('in_ifelse_block'):
                self.pop_state()
            else:
                raise ParserSyntaxError('Found an endif without matching if at line %d'%lineno)
            return

        m=re.match(r'^\s*\@\*\*\s*insert\s*(\S.*?)\s*$',line)
        if m:
            if self.active:
                contents=self.require_file(m.group(1))
                self._write(contents)
            return

        m=re.match(r'^\s*\@\*\*\s*include\s*(\S.*?)\s*$',line)
        if m:
            if self.active:
                ffilename=m.group(1)
                contents=self.require_file(ffilename)
                self.parse_lines(contents,ffilename)
            return

        m=re.match(r'^\s*\@\*\*.*',line)
        if m:
            raise ParserSyntaxError('Invalid \@** directive in line \"%s\".  Ignoring line.\n'%(line,))
        
        if self._states and not self.active: 
            return # inside a disabled block

        # Replace text of the form @[VARNAME] with the contents of the
        # respective environment variable:
        (outline,n)=re.subn(r'\@\[((?:\n|[^\]])*)\]',
                    lambda x: self.require_data(x.group(0)[2:-1]),
                    line)
        if not isinstance(outline,basestring):
            raise TypeError('The re.subn returned a %s %s instead of a basestring.'%(type(outline).__name__,repr(outline)))
        self._write(outline)
        if lineno>self.max_lines:
            raise ParserLineLimit('Read past max_lines=%d lines from input file.  Something is probably wrong.'%self.max_lines)
