using Pkg
pkg"activate ."

using Random
using Distributions
using Gadfly
using LinearAlgebra

# FIXME: This doesn't seem to give reproducibility
rng = MersenneTwister(1234);

# T = 500;
T = 50;

deltaT = 0.01;
g  = 9.81;

qc1 = 0.0001;

bigQ = [ qc1 * deltaT^3 / 3 qc1 * deltaT^2 / 2;
         qc1 * deltaT^2 / 2       qc1 * deltaT
         ];

bigR  = [0.01];

x = zeros(T + 1, 2);
y = zeros(T,     1);

x[1, :] = [0.01 0];

for t = 2:T+1
    epsilon = rand(MvNormal(zeros(1), bigR));
    y[t - 1, :] = sin(x[t- 1, 1]) .+ epsilon
    x1 = x[t - 1, 1] + x[t - 1, 2] * deltaT;
    x2 = x[t - 1, 2] - g * sin(x[t - 1, 1]) * deltaT;
    eta = rand(MvNormal(zeros(2), bigQ));
    xNew = [x1, x2] .+ eta;
    x[t, :] = xNew;
end

plot(layer(y = x[1:T,1], Geom.line, Theme(default_color=color("red"))), layer(y = y[1:T,1], Geom.point))

function ff(x, g)
    x1 = x[1] + x[2] * deltaT;
    x2 = x[2] - g * sin(x[1]) * deltaT;
    [x1, x2];
end

function f_g(x, k)
    map(y -> ff(y, k), mapslices(y -> [y], x, dims = 1))
end

function h(x)
    map(z -> sin(z[1]), mapslices(y->[y], x, dims=1))
end

nx = 2;

n_th = 1;

function prior_pdf(theta)
    pdf(MvLogNormal(zeros(length(theta)), Matrix{Float64}(I, length(theta), length(theta))), theta)
end

function prior_sample(n)
    rand(MvLogNormal(zeros(n), Matrix{Float64}(I, n, n)))
end

N = [5 30 80];
K = [100 1000 10000];
Km = [3 3 3];
N_th = [10 100 1000];

function resample_systematic( w )
    N = length(w);
    Q = cumsum(w);
    T = collect(range(0, stop = 1 - 1 / N, length = N)) .+ rand(1) / N;
    append!(T, 1);
    ix = zeros(Int64, (1, N));
    i=1;
    j=1;
    while (i <= N)
        if (T[i] < Q[j])
            ix[i] = j;
            i = i + 1;
        else
            j = j + 1;
        end
    end
    ix;
end

function resample_stratified( weights )

    N = length(weights)
    # make N subdivisions, and chose a random position within each one
    positions =  (rand(N) + collect(range(0, N - 1, length = N))) / N

    indexes = zeros(Int64, N)
    cumulative_sum = cumsum(weights)
    i, j = 1, 1
    while i <= N
        if positions[i] < cumulative_sum[j]
            indexes[i] = j
            i += 1
        else
            j += 1
        end
    end
    return indexes
end

function k(x)
    f_g(x, g)
end

function pf(inits, N, f, h, y, Q, R, nx)

    T = length(y)
    log_w = zeros(T,N);
    x_pf = zeros(nx,N,T);
    x_pf[:,:,1] = inits;
    wn = zeros(N);

    for t = 1:T
        if t >= 2
            a = resample_stratified(wn);
            x_pf[:, :, t] = hcat(f(x_pf[:, a, t-1])...) + rand(MvNormal(zeros(nx), Q), N)
        end
        log_w[t, :] = logpdf(MvNormal(y[t, :], R), h(x_pf[:,:,t]));
        wn = map(x -> exp(x), log_w[t, :] .- maximum(log_w[t, :]));
        wn = wn / sum(wn);
    end

    log_W = sum(map(log, map(x -> x / N, sum(map(exp, log_w[:, :]), dims=2))));

    return(x_pf, log_w, log_W)

end

# N = 50;
N = 49;

inits = zeros(nx, N);
inits[1, :] .= 0.01;
inits[2, :] .= 0.00;

(foo, bar, baz) = pf(inits, N, k, h, y, bigQ, bigR, nx);

plot(layer(y = x[1:T,1], Geom.line, Theme(default_color=color("red"))), layer(y = map(x -> x / 50, sum(foo[1,:,:],dims=1)), Geom.line))

function pmh(inits, K, N, n_th, y, f_g, g, nx, prior_sample, prior_pdf, Q, R)

    T = length(y);
    theta = zeros(n_th, K+1);
    log_W = -Inf;
    # FIXME:
    x_pfs = zeros(nx, N, T, K);

    while log_W == -Inf # Find an initial sample without numerical problems
        theta[:, 1] = 9 .+ prior_sample(1);
        # FIXME:
        log_W = pf(inits, N, (x) -> f_g(x, theta[:, 1][1]), g, y, Q, R, nx)[3];
    end

    for k = 1:K
        theta_prop = map(exp, map(log, theta[:, k]) + 0.1 * rand(MvNormal(zeros(n_th), 1), 1)[1, :]);
        # log_W_prop = pf(inits, N, (x) -> f_g(x, theta_prop[1]), g, y, Q, R, nx)[3];
        (a, b, c) = pf(inits, N, (x) -> f_g(x, theta_prop[1]), g, y, Q, R, nx);
        log_W_prop = c;
        x_pfs[:, :, :, k] = a;
        mh_ratio = exp(log_W_prop - log_W) * prior_pdf(theta_prop) / prior_pdf(theta[:,k]);

        display([theta[:, k], theta_prop, log_W, log_W_prop, mh_ratio, prior_pdf(theta_prop)]);

        if isnan(mh_ratio)
            alpha = 0;
        else
            alpha = min(1,mh_ratio);
        end

        dm = rand();
        if dm < alpha
            theta[:, k+1] = theta_prop;
            log_W = log_W_prop;
            new = true;
        else
            theta[:, k+1] = theta[:, k];
            new = false;
        end

        # if new == true;
        #     display(["PMH Sampling ", k, ": Proposal accepted!"]);
        # else
        #     display(["PMH Sampling ", k, ": Proposal rejected"]);
        # end
    end
    return (x_pfs, theta);
end

# (fuu, fee) = pmh(inits, K[1], N, n_th, y, f_g, h, nx, prior_sample, prior_pdf, bigQ, bigR)
(fuu, fee) = pmh(inits, K[1], N, n_th, y, f_g, h, nx, prior_sample, prior_pdf, bigQ, bigR)

plot(layer(y = x[1:T,1], Geom.line, Theme(default_color=color("red"))), layer(y = map(x -> x / 50, sum(fuu[1,:,:,1],dims=1)), Geom.line), layer(y = map(x -> x / 50, sum(fuu[1,:,:,50],dims=1)), Geom.line, Theme(default_color=color("green"))))

function smc2(inits, Km, N_th, n_th, y, f_g, g, nx, prior_sample, prior_pdf, Q, R)

    T    = length(y);
    x_th = zeros(n_th, N_th, T+1);

    x_th[:, :, 1] = prior_sample(N_th);

    N_x = zeros(T, 1);
    N_x = map(x -> max(30, x), 1:T);

    N     = maximum(N_x);
    log_w = zeros(T, N, N_th);
    x_pf  = zeros(nx, N, T, N_th);
    w_th  = zeros(N_th,T);

    for t = 1:T

        for m = 1:N_th
            if t >= 2
                wn = zeros(1:N_x[t-1]);
                wn[1:N_x[t-1]] = map(exp, log_w[t-1, 1:N_x[t-1], m] .- maximum(log_w[t-1, 1:N_x[t-1], m]));
                wn[1:N_x[t-1]] = wn[1:N_x[t-1]] / sum(wn[1:N_x[t-1]]);
                a = resample_stratified(wn);
                x_pf[:, 1:N_x[t], t, m) = f_g(x_pf[:, a, t-1, m], x_th[:, m, t]) + sqrt(Q)*randn(nx,N_x(t));
            end
            log_w[t, 1:N_x[t], m] = logpdf(MvNormal(y[t, :], R), h(x_pf[:, 1:N_x[t], t, m]));
        end

        if t >= 2
            # w_incr = squeeze(sum(1/N_x(t)*exp(log_w(t,1:N_x(t),:)),2));
            # w_th(:,t) = w_th(:,t-1).*w_incr;
            # cum_log_w = cum_log_w + log(w_incr);
        else
            w_th[:, 1] = dropdims(sum(1 / N_x[t] * map(exp, log_w[t,1:N_x[t],:]), dims = 1), dims = 1);
            cum_log_w = map(log, w_th[:, 1]);
        end

        w_th[:, t] = w_th[:, t] ./ sum(w_th[:, t]);
        ess = (sum(w_th[:, t]))^2 / sum(w_th[:, t].^2);

#     if ESS<0.3*N_th

#         a = systematic_resampling(w_th(:,t),N_th);
#         x_pf(:,1:N_x(t),t,:) = x_pf(:,1:N_x(t),t,a);
#         log_w(t,1:N_x(t),:) = log_w(t,1:N_x(t),a);
#         x_th(:,:,t) = x_th(:,a,t);
#         cum_log_w = cum_log_w(a);
#         w_th(:,t) = 1/N_th;

#         for k = 1:Km

#             for m = 1:N_th

#                 if k == 1
#                     x_th_prop = x_th(:,m,t) + 0.1*randn(n_th,1);
#                 else
#                     x_th_prop = x_th(:,m,t+1) + 0.1*randn(n_th,1);
#                 end

#                 [log_W_prop, x_pf_prop, log_w_prop] = pf( N_x(t), @(x,u)f_g(x,u,x_th_prop), g, u(1:t), y(1:t,:), Q, R, nx);

# %                 cum_log_w(m) = pf( N_x(t), @(x,u)f_g(x,u,x_th(:,m,t)), g, u(1:t), y(1:t,:), Q, R, nx);

#                 dm = rand;
#                 mh_ratio = exp(log_W_prop-cum_log_w(m))*prior_pdf(x_th_prop)/prior_pdf(x_th(:,m,t));
#                 if isnan(mh_ratio)
#                     alpha = 0;
#                 else
#                     alpha = min(1,mh_ratio);
#                 end
#                 if dm < alpha
#                     x_th(:,m,t+1) = x_th_prop;
#                     cum_log_w(m) = log_W_prop;
#                     x_pf(:,1:N_x(t),t,m) = x_pf_prop;
#                     log_w(t,1:N_x(t),m) = log_w_prop;
# %                     disp('a!')
#                 else
#                     if k == 1
#                         x_th(:,m,t+1) = x_th(:,m,t);
#                     else
#                         x_th(:,m,t+1) = x_th(:,m,t+1);
#                     end
# %                     disp('r')
#                 end
#             end
#             display(['Propagating with PMH. Step ', num2str(k), '/', num2str(Km)])
#         end

#     else
#         x_th(:,:,t+1) = x_th(:,:,t);
#     end

        clf
        scatter(t*ones(N_th,1),x_th(1,:,t),w_th(:,t)*10000+1,'r','filled')
        xlim([1 T])
        drawnow
        display(['SMC^2, t = ', num2str(t)])
    end
end



# for i = 1:3
#     tic
#     theta{i} = pmh( K(i), N(i), n_th, u, y, f_g, g, nx, prior_sample, prior_pdf, Q, R); %#ok
#     pmh_time(i) = toc; %#ok

#     tic
#     [ x_th1, w_th1 ] = smc2( Km(i), N_th(i), n_th, u, y, f_g, g, nx, prior_sample, prior_pdf, Q, R);
#     x_th{i} = x_th1; %#ok
#     w_th{i} = w_th1; %#ok
#     smc2_time(i) = toc; %#ok

# end

# %%

# hist_v = 0.05:0.05:0.85;

# figure(4), clf
# for i = 1:3
#     a = systematic_resampling(w_th{i}(:,T),N_th(i));
#     x_th_f = x_th{i}(:,a,T);
#     subplot(3,3,3*i-1)
#     [by,bx] = hist(x_th_f(1,:),hist_v);

#     ind = find(by>0,1,'first'):find(by>0,1,'last');
#     by = by(ind); bx = bx(ind);

#     th_ml = 0.54;

#     bdx = diff(bx); bdx = bdx(1);
#     hxy = [bx(1)-bdx/2 0];
#     by = by/N_th(i);

#     for j=1:numel(bx)
#         hxy = [hxy; [hxy(end,1) by(j)]; [bx(j)+bdx/2 by(j)]]; %#ok
#     end

#     hxy = [hxy; [hxy(end,1) 0]; hxy(1,:)];
#     fill(hxy(:,1),hxy(:,2),1, ...
#     'EdgeColor',[.6 .0 .0],'FaceColor',[.6 .0 .0],'LineWidth',0.01);
#     hold on;
#     set(gca,'xTick',sort([0:0.2:0.8 th_ml]))
#     set(gca,'xTickLabel',{'0','0.2','0.4','ML','','0.8'})
#     if i == 1; title('SMC^2'); end
#     xlim([min(hist_v)-0.05 max(hist_v)+0.05])
#     xlabel('beta')

#     burn_in = round(min(K(i)/2,500));
#     subplot(3,3,3*i)
#     [by,bx] = hist(theta{i}(1,burn_in:end),hist_v);

#     ind = find(by>0,1,'first'):find(by>0,1,'last');
#     by = by(ind); bx = bx(ind);

#     bdx = diff(bx); bdx = bdx(1);
#     hxy = [bx(1)-bdx/2 0];
#     by = by/(K(i)-burn_in);

#     for j=1:numel(bx)
#         hxy = [hxy; [hxy(end,1) by(j)]; [bx(j)+bdx/2 by(j)]]; %#ok
#     end

#     hxy = [hxy; [hxy(end,1) 0]; hxy(1,:)];
#     h = fill(hxy(:,1),hxy(:,2),1, ...
#     'EdgeColor',[.6 .0 .0],'FaceColor',[.6 .0 .0],'LineWidth',0.01);
#     hold on;
#     set(gca,'xTick',sort([0:0.2:0.8 th_ml]))
#     set(gca,'xTickLabel',{'0','0.2','0.4','ML','','0.8'})
#     xlim([min(hist_v)-0.05 max(hist_v)+0.05])
#     if i == 1; title('PMH'); end
#     xlabel('\beta')
#     axis_v = axis;

#     subplot(3,3,3*i-1)
#     axis(axis_v)

#     subplot(3,3,3*i-2)
#     text(0,0.7,['PMH: K = ', num2str(K(i)), ', N = ', num2str(N(i)), ' (',num2str(pmh_time(i)/60,1),' min)' ])
#     text(0,0.3,['SMC^2: N_ theta = ', num2str(N_th(i)), ', K_m = ', num2str(Km(i)), ' (',num2str(smc2_time(i)/60,1),' min)' ])
#     axis off

# end
# %%
# figure(3), clf
# t_vec = [1 20:20:T];
# N_th_s = 5;
# ap = systematic_resampling(1/N_th(i)*ones(1,N_th(i)),N_th_s);
# for t = t_vec
#     scatter(t*ones(N_th_s,1),x_th{3}(1,ap,t),w_th{3}(ap,t)*10000+1,[0.6 0 0],'filled')
#     hold on
# end
# ylim([-0.5 1.5])
# set(gca,'xTick',t_vec)
# xlabel('Time t')
# ylabel('beta_t')

# %%
# figure(3), clf
# t_vec = [1 20:20:T];
# N_th_s = 50;
# ap = systematic_resampling(1/N_th(i)*ones(1,N_th(i)),N_th_s);
# i = 3;
# for t = t_vec
#     scatter(t*ones(N_th_s,1),x_th{i}(1,ap,t),w_th{i}(ap,t)*10000+1,[0.6 0 0],'filled')
#     hold on
# end
# ylim([-0.5 1.5])
# set(gca,'xTick',t_vec)
# xlabel('Time t')
# ylabel('beta_t')

# %%
# figure(2), clf

# i = 3;
# plot(theta{i}(1:min(2000,K(i))),'linewidth',1,'color',[0.6 0 0])
# set(gca,'xTick',0:1000:2000)
# xlabel('Iteration k')
# ylabel('beta[k]')
indx = zeros(1, N);
