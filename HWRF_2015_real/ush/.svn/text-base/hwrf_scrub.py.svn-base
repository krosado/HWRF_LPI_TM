#! /bin/env python

import logging, os, shutil, sys
import produtil.setup, produtil.fileop, produtil.log

from produtil.log import jlogger

class WillNotDelete(Exception):
    """Raised by various safety checks if someone tries to delete
    something they should not, such as "/"."""

class Deleter(object):
    def __init__(self,logger):
        self.__logger=logger
        self.__rmtrees=set()
        self.__rmdirs=set()
        self.badflag=False
    @property
    def logger(self):
        return self.__logger

    def validate_path(self,norm):
        if not os.path.exists(norm):
            return # cannot validate if it does not exist
        if os.path.ismount(norm):
            raise WillNotDelete('%s: is a mount point (fs root)'%(norm,))
        if os.path.samefile('/',norm):
            raise WillNotDelete('%s: is same as /'%(norm,))
        for var in ( 'HOMEhwrf', 'USHhwrf', 'EXhwrf', 'PARMhwrf', 
                     'FIXhwrf', 'FIXgsi' ):
            if var in os.environ:
                vardir=os.environ[var]
                if vardir=='': continue
                if os.path.samefile(os.environ[var],norm):
                    raise WillNotDelete('%s: same as $%s'%(norm,var))

    def add(self,dirname):
        """Adds a directory to the list to be deleted.  The directory
        is passed through various safeguards first."""
        norm=produtil.fileop.norm_expand_path(dirname,fullnorm=True)
        self.logger.info('%s: normalizes to %s'%(dirname,norm))
        self.validate_path(norm)
        self.logger.info('%s: will recursively delete this'%(norm,))

        parent=os.path.dirname(dirname)
        self.logger.info('%s: will rmdir this if it is empty'%(parent,))

        self.__rmdirs.add(parent)
        self.__rmtrees.add(dirname)

    def _rmtree_onerr(self,function,path,exc_info):
        """This is an internal implementation function called by
        shutil.rmtree when an underlying function call failed.  See
        the Python documentation of shutil.rmtree for details."""
        self.logger.warning('%s: %s failed: %s'%(
                str(path),str(function),str(exc_info)))
        self.badflag=True

    def rmtree(self,tree):
        """Deletes the tree, if possible."""
        try:
            # If it is a file, special file or symlink we can just
            # delete it via unlink:
            os.unlink(tree)
            return
        except EnvironmentError as e:
            pass
        # We get here for directories.
        self.logger.info('%s: rmtree'%(tree,))
        shutil.rmtree(tree,ignore_errors=False,onerror=self._rmtree_onerr)

    def have_dirs(self):
        return len(self.__rmdirs)>0

    def swap_dirs(self):
        dirs=self.__rmdirs
        self.__rmdirs=set()
        return dirs

    def go(self,max_rmdir_loop=30):
        logger=self.logger
        logger.info('Delete files: first pass.')
        self.badflag=False
        for tree in self.__rmtrees:
            self.rmtree(tree)

        if self.badflag:
            logger.warning('Some deletions failed.  Will try again.')
            logger.info('Delete files: second pass.')
            self.badflag=False
            for tree in self.__rmtrees:
                if os.path.exists(tree):
                    self.rmtree(tree)
                else:
                    logger.info('%s: already gone.'%(tree,))

        if self.badflag:
            logger.error('Could not delete files after two tries.')

        logger.info('Remove parent and ancestor directories.')
        iloop=0
        while iloop<max_rmdir_loop and self.have_dirs():
            iloop+=1
            dirs=self.swap_dirs()
            for dir in dirs:
                try:
                    logger.info('%s: rmdir'%(dir,))
                    os.rmdir(dir)
                except EnvironmentError as e:
                    logger.info('%s: cannot remove: %s'%(dir,repr(e)))
                    continue
                try:
                    parent=os.path.dirname(dir)
                    self.__rmdirs.add(parent)
                except EnvironmentError as e:
                    logger.warning('%s: cannot determine parent'%(dir,))
                    continue # can ignore this error
        if iloop>=max_rmdir_loop:
            logger.warning(
                'Hit maximum loop count of %d.  Some ancestor directories '
                'may still exist.'%(max_rmdir_loop,))
                    
def main():
    logger=logging.getLogger('hwrf_scrub')
    scrubber=Deleter(logger)
    for arg in sys.argv[1:]:
        scrubber.add(arg)
    scrubber.go()
    
if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HWRF scrubber is aborting: '+str(e),exc_info=True)
        sys.exit(2)

