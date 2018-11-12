function PresentPause(wPtr, P)
% Does exactly what the name says.
disp('Pause')
WaitSecs(0.25);

%% Present the pause.
pausetext = ['Pause\n\nWenn Sie bereit sind,\n'...
    'drücken Sie die Leertaste um fortzufahren.'];
Screen(wPtr, 'TextSize', 24);
tw = RectWidth(Screen('TextBounds', wPtr, pausetext));
DrawFormattedText(wPtr, pausetext, 'center','center',255);
Screen('Flip', wPtr);

%% wait until key is pressed
[~,keyCode] = KbWait();
if keyCode == P.Quitkey
    CloseAndCleanup(P)
end
fprintf('\n');
WaitSecs(0.5);

end
