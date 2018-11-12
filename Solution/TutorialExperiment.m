function [Info] = TutorialExperiment
% Code for the simplest Experiment in the world.
%% ----------------------------------------------------------------
% 1. Load parameters.
% -------------------------------------------------------------------------
addpath('Functions');

%get Subjectnumber
answer = inputdlg({'Subject (Number: 01-99)'},...
                   'Please enter number');
Name = answer{1};

% P transports all parameters (see parameters.m for what those are...)
P = Parameters;
Info.P = P;

%% ----------------------------------------------------------------
% 2. Open display.
% -------------------------------------------------------------------------
Screen('Preference', 'SkipSyncTests', 1);
Screen('ConfigureDisplay', 'Scanout', P.PresentScreen, 0, P.myWidth,...
        P.myHeight, P.myRate);
wPtr = Screen('OpenWindow', P.PresentScreen, P.BgColor,  [0 0 640 320]);

%% ----------------------------------------------------------------
% 3. Open connections to Eyetracker
% -------------------------------------------------------------------------
[P, wPtr] = EyelinkStart(P, wPtr,['TE_', Name]);
EyelinkNotify('Ruebezahl!', P.isET);

%% ----------------------------------------------------------------
% 4. Welcome screen
% -------------------------------------------------------------------------
msg = 'Please do the task!\n\nPress any key to proceed!';
DrawFormattedText(wPtr, msg, 'center', 'center');
Screen('Flip', wPtr);
WaitSecs(0.25);
KbWait;

%% ----------------------------------------------------------------
% 5. Run across trials.
% -------------------------------------------------------------------------

%try-catch to assure that connection to Eyelink is closed even if the
%Experiment crashes.
try
    for t = 1:P.ntrials
        % send status to EL-host-PC-Screen
        EyelinkNotify(sprintf('Trial %i/%i', t, P.ntrials), P.isET);
        if P.isET
            % Send TrialID to tracker. This is used in Eyelink's analysis
            % software. (so not necessary, if you're not going to use that)
            % check http://download.sr-support.com/dispdoc/page8.html for
            % the whole convention. Some are included in EyelinkStart. I'll
            % ignore the rest of them for now.
            Eyelink('Message', 'TRIALID %d', t);
        end
        % Run a trial.
        Info = OneTrial(t, P, wPtr);
        if Info.isQuit
            break
        end
        
        % Create a tab-delimited message that contains the info of the
        % current trial. R and Matlab can later create a table with all
        % info. This makes coregistration with behavioral data
        % redundant.
        EyelinkSendTabMsg(t, Name, Info.Response, Info.Correct,...
            Info.isQuit, Info.FixationOnsetTime, Info.ResponseScreenOn,...
            Info.Truth, Info.didrecal, 'eyelinkconnected', P.isET)
    end
    
%% ----------------------------------------------------------------
% 6. shutdown connections, and close
% -------------------------------------------------------------------------
catch ME
   CloseAndCleanup(P);
   rethrow(ME);
end
CloseAndCleanup(P);