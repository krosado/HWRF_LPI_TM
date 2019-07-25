"""This module contains utilities for plugging HWRF into the Rocoto
workflow manager."""
__all__=['cycles_as_entity']

import StringIO, random
import hwrf.numerics

from hwrf.numerics import to_datetime, to_timedelta

epsilon=to_timedelta(5)
six_hours=to_timedelta(6*3600)

sanity_quotes=list([ 
        '''"I became insane, with long intervals of horrible sanity."
 -- Edgar Allan Poe''',
        '''"Sanity and happiness are an impossible combination."
 -- Mark Twain''',
        '''"One person's craziness is another person's reality."
 -- Tim Burton''',
        '''"Don't worry. You're just as sane as I am."
 -- J.K. Rowling, "Harry Potter and the Order of the Phoenix"''',
        '''"Here lies a nuisance dedicated to sanity." -- David Low''',
        '''"Sanity is only that which is within the frame of reference of 
conventional thought." -- Erich Fromm''',
        '''"If you think anyone is sane you just don't know enough about 
them." -- Christopher Moore, "Practical Demonkeeping"''',
        '''"I mean, maybe I am crazy. I mean, maybe. But if this is all 
there is, then I don't want to be sane."
 -- Neil Gaiman, "Neverwhere"''',
        '''"Show me a sane man and I will cure him for you." -- C.G. Jung''',
        '''"When you're the only sane person, you look like the only insane 
person." -- Criss Jami, "Diotima, Battery, Electric Personality"''',
        '''"When the whole world is crazy, it doesn't pay to be sane."
 -- Terry Goodkind, "The Pillars of Creation"''',
        '''"Sanity is a cozy lie." -- Susan Sontag''',
        '''"One must be sane to think clearly, but one can think deeply and 
be quite insane." -- Nikola Tesla''',
        '''"When dealing with the insane, the best method is to pretend to 
be sane." -- Hermann Hesse''',
        '''"Perfect sanity is a myth propagated by straitjacket salesmen."
 -- Rebecca McKinsey''',
        '''"As long as you doubt your sanity, you can't be insane."
 -- Miles Keaton Andrew''',
        '''"Sometimes, to regain sanity, one had to acknowledge and embrace
the madness." -- Morgan Rhodes, "Rebel Spring"''',
        '''"Sanity is transmuting all the insane parts of your brain into 
a creative outlet" -- Jaeda DeWalt''',
        '''"Reality is a hallucination shared by most sane men."
 -- Mokokoma Mokhonoana''',
        '''"Sanity? Sorry, but I don't remember having such a useless 
thing in the first place" -- Tite Kubo''' ])
"""A set of famous quotes about sanity.  These are printed by the
sanity_check_failed function after a failure of a sanity check.  The
purpose is to give the user a measure of emotional support after
failure to configure an inenvitably complex system."""

def entity_quote(string):
    """Returns a copy of the string with & " < % and > replaced with
    their respective XML entities &#38; &#34; &#60; &#37; and &#62;"""
    return string.replace('&','&#38;') \
                 .replace('"','&#34;') \
                 .replace('<','&#60;') \
                 .replace('%','&#37;') \
                 .replace('>','&#62;')

def sanity_check_failed(logger,ex):
    """Logs information about a failure of a sanity check routine.
    The failure is in exception ex, and the "logger" argument must be
    a logging.Logger."""
    quote=sanity_quotes[random.randint(0,len(sanity_quotes)-1)]
    logger.critical('Sanity check failed.',exc_info=True)
    logger.info('\n'+quote+'\n'
        'HWRF SANITY CHECK FAILED.  Cannot run this configuration.\n'
        'Check paths and conf files.  See earlier messages for details.')

def cycles_as_entity(cycleset):
    """Returns a set of Rocoto XML <cycledef> tags to add to an XML
    file.  The tags will define the list of cycles specified in the
    cycleset argument.  That argument must be a set of datetime
    objects."""
    cycles=list(cycleset)
    cycles = [ to_datetime(x) for x in cycles ]
    cycles.sort()
    ctream=StringIO.StringIO()
    first=cycles[0]
    last=cycles[0]
    sent=cycles[0]-six_hours
    for cycle in cycles:
        if to_datetime(cycle) > to_datetime(last)+six_hours+epsilon:
            # Found a break in the cycles
            writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                %(first.strftime('%Y%m%d%H'),
                  last.strftime('%Y%m%d%H'))
            ctream.write(writeme)
            sent=last
            first=cycle
            last=cycle
        else:
            last=cycle
    if sent+epsilon < last:
        # Need to send the last group of cycles
        writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                     %(first.strftime('%Y%m%d%H'),
                       last.strftime('%Y%m%d%H'))
        ctream.write(writeme)
    ctream.write('\n')

    out=str(ctream.getvalue())
    ctream.close()
    return out

