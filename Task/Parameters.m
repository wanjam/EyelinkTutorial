function [P] = Parameters
%% -----------------------------------------------------------------------
% Make system-specific adjustments
%  -----------------------------------------------------------------------
nscreens        = Screen('Screens');
myres           = Screen('Resolution', max(nscreens));
P.PresentScreen = max(nscreens); 
P.myWidth       = myres.width;
P.myHeight      = myres.height;
P.myRate        = myres.hz;
P.isET          = 0; %Eyetracker  
P.trackr.dummymode = 0; %set this to 1 to try EL-functionality on other devices.
P.sz            = [52.2 29.4]; % monitor size in cm
P.vdist         = 86; % distance of oberver from monitor

%% ------------------------------------------------------------------------
% Parameters of the display.
% -------------------------------------------------------------------------
P.CenterX      = P.myWidth/2;
P.CenterY      = P.myHeight/2;
P.Center       = [P.CenterX, P.CenterY];
P.res          = [P.myWidth P.myHeight];
[P.pixperdeg, P.degperpix] = VisAng(P);
P.BgColor    = [127, 127, 127];

%% ------------------------------------------------------------------------
% Response keys
% -------------------------------------------------------------------------
KbName('UnifyKeyNames');    
RestrictKeysForKbCheck([]);
P.Quitkey = KbName('ESCAPE');

%% ----------------------------------------------------------------
% Parameters of the procedure
% -------------------------------------------------------------------------
P.BreakAfternTrials = 3;
P.ntrials = 1;

%% ----------------------------------------------------------------
% Parameters for controlling fixations
% -------------------------------------------------------------------------
P.minFixTime = 2.000;
P.maxUntilFixTime = 3.000;
P.maxDegDeviation = 3.5;
P.recalIfNoFixation = 1;
P.IgnoreBlinks = 0;

%% ----------------------------------------------------------------
% Eye-tracking calibration target locations
% -------------------------------------------------------------------------
% % for this example, we assume that we're using 1024 * 768 pixel pictures
% P.PicWidth = 1024;
% P.PicHeight = 768;
% 
% %x1,y1,[...],x9,y9 ;must be nine x-y pairs
% X = P.CenterX;
% A = P.PicWidth/2-20;
% Y = P.CenterY;
% B = P.PicHeight/2-20;
% P.CalibLocations = [X-A,Y-B,  X,Y-B,  X+A,Y-B,...
%                     X-A,Y,    X,Y,    X+A,Y,...
%                     X-A,Y+B,  X,Y+B,  X+A,Y+B]; 
                
%% ----------------------------------------------------------------
% Eye-tracking messages
% -------------------------------------------------------------------------
P.etmsg_FixOn       = 'FixOn';
P.etmsg_TrlOn       = 'TrlOn';
P.etmsg_FeedbackOn  = 'FeedbackOn';
P.etmsg_FeedbackOff = 'FeedbackOff';
P.etmsg_hsmvd       = 'hsmvd';

end
