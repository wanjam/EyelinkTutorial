function Info = OneTrial(t, P, wPtr)

isQuit = 0;
%% ----------------------------------------------------------------
% Set defaults for output
% -------------------------------------------------------------------------
Info.isQuit = isQuit;
Info.Response = NaN;
Info.Correct = NaN;
Info.FixationOnsetTime = NaN;
Info.ResponseScreenOn = NaN;
Info.didrecal = NaN;
Info.Truth = NaN;

%% ----------------------------------------------------------------
% Do a recalibration/validation of the eyetracker after pause
% -------------------------------------------------------------------------
if(mod(t, P.BreakAfternTrials) == 1)
    if t~=1
        PresentPause(wPtr, P, Info, t);
        EyelinkRecalibration(P, P.isET);
    end
end


%% ----------------------------------------------------------------
% Fixation target
% -------------------------------------------------------------------------
% draw the best fixation target (Thaler et al, 2013)
my_optimal_fixationpoint(wPtr, P.CenterX, P.CenterY, 0, 127, P.pixperdeg);
TrialOnsetTime = Screen('Flip', wPtr);

% Send trigger. Eyelink triggering is much faster and does not need to
% pause. Thus, **always** send Eyelink triggers first, EEGtriggers second!
EyelinkSendTabMsg(P.etmsg_FixOn, 'eyelinkconnected', P.isET);

%% ----------------------------------------------------------------
% control fixation
% -------------------------------------------------------------------------
[didrecal, FixationOnset, hsmvd] = EyelinkControlFixation(P,...
    P.minFixTime, P.maxUntilFixTime, P.Center, P.maxDegDeviation, P.isET,...
    P.pixperdeg, P.recalIfNoFixation, P.IgnoreBlinks);

if P.isET == 0
    WaitSecs(2); % So we have a chance to see the fixationpoint
end
% you could use the FixationOnset Time to calculate when to do the next
% flip.

%% ----------------------------------------------------------------
% Response Screen
% -------------------------------------------------------------------------
msg = sprintf(['Did you need more than %.3f seconds to fixate the ',...
    'target?\nPress ''y'' or ''n''!'], P.maxUntilFixTime * 0.5);
DrawFormattedText(wPtr, msg, 'center', 'center');
ResponseScreenOn = Screen('Flip', wPtr);
WaitSecs(0.25);
[~, keyCode] = KbWait;
if keyCode(KbName('y'))
    Response = 'yes';
elseif keyCode(P.Quitkey)
    isQuit = 1;
else
    Response = 'no';
end

% check if response was correct
if ((FixationOnset - TrialOnsetTime) / P.maxUntilFixTime) > 0.5
    Truth = 'yes';
else
    Truth = 'no';
end

%% ----------------------------------------------------------------
% Feedback
% -------------------------------------------------------------------------
if hsmvd
    msg = 'It''s the optimal fixation point!\n Next time: fixate!!';
else
    msg = 'Awesome!\nYou''re so good at fixating things!!';
end
DrawFormattedText(wPtr, msg, 'center', 'center');
Screen('Flip', wPtr);
WaitSecs(0.25);
[~, keyCode] = KbWait;
if keyCode(P.Quitkey)
    isQuit = 1;
end


%% ----------------------------------------------------------------
% Gather everything in an info struct
% -------------------------------------------------------------------------
Info.isQuit = isQuit;
Info.Response = Response;
Info.Correct = strcmp(Response, Truth);
Info.FixationOnsetTime = FixationOnset-TrialOnsetTime;
Info.ResponseScreenOn = ResponseScreenOn-TrialOnsetTime;
Info.didrecal = didrecal;
Info.Truth = Truth;

end
