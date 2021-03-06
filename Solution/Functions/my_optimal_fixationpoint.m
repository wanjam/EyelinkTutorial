function my_optimal_fixationpoint(window, x, y, fillcolor, bgcolor, ppd, theTex, diameter)
% my_optimal_fixationpoint(window, x, y, diameter, fillcolor);
%
% Creates an optimal fixation point, as outlined in:
% Thaler, L., Schütz, A. C., Goodale, M. A., & Gegenfurtner, K. R. (2013). 
%    What is the best fixation target? The effect of target shape on 
%    stability of fixational eye movements. Vision Research, 76, 31-42.
% 
% window: window pointer
% x: x coordinate
% y: y coordinate
% diameter: size of the circle (defaults to 0.6 degree, as outlined in the paper)
% fillcolor: RGB for the figure's color. If just one value, it's recycled 
%            as triplet. Special flag 'grating' makes the target a
%            black/white grating
% theTex: if fillcolor=='grating', a grating needs to be specified in
%         advance. possibly by calling getSimpleGrating.m in parameters.m
%   
% bgcolor: should be the same as overallbackground
% ppd: pixels per degree
%
% Wanja Moessing, May 2016

% make sure, that the lines can be drawn smoothly/ anti-aliased
Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

% convenience, so calls are shorter
if length(fillcolor)==1 % color of the two circles [R G B]
    [fillcolor1,fillcolor2] = deal([fillcolor fillcolor fillcolor]);
elseif length(fillcolor)==3
    [fillcolor1,fillcolor2] = deal(fillcolor);
end
if strcmp('grating',fillcolor)
    fillcolor2 = [0,0,0];
end
if length(bgcolor)==1 % color of the Cross [R G B]
    bgcolor = [bgcolor bgcolor bgcolor];
end

if ~exist('diameter','var')
    d1 = 0.6; % diameter of outer circle (degrees)
    d2 = 0.2; % diameter of inner circle (degrees); 0.2° would be the version of the paper; however, our setup results in a max linewidth of .181246°
else
    d1 = diameter;
    d2 = diameter/3;
end

if ~strcmp('grating',fillcolor)
    Screen('FillOval', window, fillcolor1, [x-d1/2 * ppd, y-d1/2 * ppd, x+d1/2 * ppd, y+d1/2 * ppd], d1 * ppd);
else
    Screen('DrawTextures', window, theTex,[],[x-d1/2 * ppd, y-d1/2 * ppd, x+d1/2 * ppd, y+d1/2 * ppd],45);
end

%dummy line to get maxSmoothLineWidth for current computer
[~, maxSmoothLineWidth, ~, maxAliasedLineWidth] = Screen('DrawLines',window);

fprintf('\nMaxSmooth: %.5f\nMaxAlias: %.5f\nDesired: %.5f', maxSmoothLineWidth, maxAliasedLineWidth, d2*ppd);
Screen('DrawLines',window,...
    [x-d1/2*ppd-2,x+d1/2*ppd+2,x,x; y,y,y-d1/2*ppd-2,y+d1/2*ppd+2],...
    min(d2*ppd,maxSmoothLineWidth), bgcolor,[],2); %-2/+2 because anti-aliasing otherwise casts a shadow ring around the whole thing

if d2*ppd>maxAliasedLineWidth
    warning('fxpt:pxvrld',['linewidth receives a cutoff because %.2f degree of visual'...
        ' angle are more than %.2fpx. (the max linewidth for this PC)\n'...
        'So the actual width of the lines between the fixation cross is: %.4f°'],d2,maxAliasedLineWidth,maxAliasedLineWidth/ppd);
    [~,msgid] = lastwarn;
    warning('off',msgid);%just show for the first fixation point
end
Screen('FillOval', window, fillcolor2, [x-d2/2 * ppd, y-d2/2 * ppd, x+d2/2 * ppd, y+d2/2 * ppd], d2 * ppd);