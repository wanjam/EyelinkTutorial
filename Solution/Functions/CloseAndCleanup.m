function CloseAndCleanup(P)
% Used for closing screens and ports after experiments finishes or crashes.
sca;
ShowCursor;

% puts the eyetracker in standby, finished writing to the file and then
% pulls the file over LAN to the display-pc
EyelinkStop(P, P.isET)
end