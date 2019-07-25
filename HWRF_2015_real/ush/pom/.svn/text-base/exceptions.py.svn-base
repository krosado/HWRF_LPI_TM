#!/usr/bin/env python

"""
Written By Samuel Trahan, EMC, NOAA for University of Rhode Island on
6/13/14.  This module contains exception classes for reporting errors
in the POM initialization.
Please report bugs/questions/comments to bijuthomas@mail.uri.edu.
"""


class POMException(Exception):
    """The base class of all exceptions relating to the POM"""

class POMInputError(POMException):
    """Raised when a requird input or input directory did not exist."""

class POMInitFailed(POMException):
    """Raised when a POM initialization program unexpectedly fails."""

class POMSSTError(POMException):
    """Raised when the init has trouble extracting SSTs."""

class POMPrepError(POMException):
    """Raised when the POM prep fails."""

class POMConfigError(POMException):
    """Raised when an impossible configuration is requested, such as
    an unspoorted tropical basin."""

class POMUnsupportedBasin(POMConfigError):
    """Raised when an unsupported basin is requested."""
