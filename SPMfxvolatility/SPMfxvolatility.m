% ---------------------------------------------------------------------
% Book:         SPM
% ---------------------------------------------------------------------
% Quantlet:     SPMfxvolatility
% ---------------------------------------------------------------------
% Description:  SPMfxvolatility plots estimated conditional variance function  
%               of FX rate EUR/USD using local polynomial estimator.
% ---------------------------------------------------------------------
% Usage:        SPMfxvolatility, depends on lpregest(), lpvarcb() and quadk()
% ---------------------------------------------------------------------
% Inputs:       exrate_GBR-USD_EUR-USD.mat
% ---------------------------------------------------------------------
% Output:       plot of conditional variance function of FX rate EUR/USD
% ---------------------------------------------------------------------
% Example:      fig 1.2 in empirical illustration 1.3
% ---------------------------------------------------------------------
% Author:       Maria Osipenko 20100423
% ---------------------------------------------------------------------

clc
clear
close all

load 'exrate_GBR-USD_EUR-USD.mat'
returns   = log((fx_data(2:end, 3))./(fx_data(1:end - 1, 3))); %FX-rates to returns 
y         = [returns(1:(end - 1), 1) returns(2:end,1)];
yy        = [y(:, 1) y(:, 2).^2];
hm        = 0.04; %rule of thumb bandwidth
hs        = 0.03;
[m1h yg]  = lpregest(y(:,1), y(:,2), 1, hm); %estimate conditional mean function
[m2h yg]  = lpregest(yy(:,1), yy(:,2), 1, hs);%estimate conditional 2. moment 
sh        = [yg m2h(1, :)'-(m1h(1, :).^2)']; %conditional variance
m1hx      = interp1(yg, m1h(1, :)', y(:, 1)); %interpolate mean
shx       = interp1(yg, sh(:, 2), y(:, 1)); %interpolate variance
shx       = [y(:, 1) shx];
[clo cup] = lpvarcb(y(:, 1), y(:, 2), m1hx, sh(:, 2),shx(:, 2), hs, 0.01);

%compute pointwise CI with alpha=0.01
shx = sortrows(shx, 1);

figure(1) %plot the results
plot(shx(:, 1), shx(:, 2),'LineWidth', 1, 'Color', [0 0 0]);

%plot the estimated conditional variance function:
title('FX Variance Function', 'FontSize', 16 ,'FontWeight', 'Bold'); 
hold all;
plot(yg, cup, 'LineWidth', 1, 'Color', [0 0 1]);
plot(yg, clo, 'LineWidth', 1, 'Color', [0 0 1]);
set(gca,'LineWidth',1.6,'FontSize',16,'FontWeight','Bold')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pay attention !!!
%% Following functions are needed to run this quantlet. 
%% You will need to save them each separately under function name in the same working directory.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% function needed to compute local polynomial regression estimator
function [bhat x] = lpregest(X,Y,p,h)
%% set parameters
n = max(size(X));
x = (min(X):(max(X) - min(X))/(100):max(X))'; %grid
m = max(size(x));
bhat = zeros(p + 1, m);
for i = 1:m
    dm = ones(n,1);
    xx = X-x(i);
    if p > 0
        for j=1:p
        dm = [dm (xx).^j];
        end;
    end;
    w = diag(quadk(xx./h)./h);
    mh = inv(dm'*w*dm)*dm'*w;
    bhat(:,i) = mh*Y;
end;
end

%% function needed to compute quadratic kernel
function y = quadk(x);

% Usage : y = quadk(x);
% Returns: y = (15/16).*(x.*x.<1).*(1- x.*x).^2 ) ;
I = (x.*x < 1) ;
x = x .*I ;
y = (15/16) * I .* (1- x.*x).^2 ;

%% function to compute confidence intervals
function [clo cup]=lpvarcb(X, Y, mhx, sh, shx, hs, alpha)
n = max(size(X));
x = (min(X):(max(X) - min(X))/(100):max(X))'; %grid
m = max(size(x));
khat = zeros(m, n);
res = ((Y - mhx).^2 - shx).^2; %find the residuals
for i = 1:n
    xx = X(i)-x;
    w = quadk(xx./hs)./hs; %compute kernel weigths
    khat(:,i) = w;
end;

sh2 = (khat*res).*1./(n*khat*ones(n,1)); %compute variance of variance
[fh ,~, h] = ksdensity(X,x); %estimate density of x
ck  = 5/7; %||K||^2_2
cl  = norminv(1 - (alpha/2)); %alpha-quantile of standard normal distribution
mrg = cl*((ck*sh2)./(fh.*h*n)).^(0.5);
clo = sh - mrg;
cup = sh + mrg;

