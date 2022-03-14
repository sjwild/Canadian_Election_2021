
colours_fed = Dict("LPC" => :red,
                   "CPC" => :blue,
                   "NDP" => :orange,
                   "BQ" => :cyan,
                   "GPC" => :green,
                   "PPC" => :purple,
                   "Other" => :yellow)

# plot trend from 2018 election
trend_fed = plot_trend(fed_results,
                       polls_fed,
                       colours_fed,
                       "Estimated vote intention of Canadian voters:\n2019 to 2022";
                       ylims = (0, .55),
                       legend = :topleft,
                       yticks = ([0.0, 0.1, 0.2, 0.3, 0.4, 0.5], 
                                  ["0", "10", "20", "30", "40", "50"]),
                        annotationy = -0.07);

# plot trend for last 180 days
trend_180_fed = plot_trend(fed_results,
                           polls_fed,
                           colours_fed,
                           "Estimated vote intention of Canadian voters:\nLast 180 days",
                           :last180;
                           ylims = (0, .60),
                           legend = :topleft,
                           yticks = ([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
                                     ["0", "10", "20", "30", "40", "50", "60"]),
                           annotationy = -0.07);


# Plot house effects
plt_house_fed, title_fed, ann_fed = plot_house(fed_results,
                                      "House effects at the federal level: 2019 to 2022")


house_effects_fed = plot(title_fed,
                         plt_house_fed[1],
                         plt_house_fed[2],
                         plt_house_fed[3], 
                         plt_house_fed[4],
                         plt_house_fed[5],
                         plt_house_fed[6],
                         plt_house_fed[7],
                         ann_fed,
                         layout = @layout([A{0.01h}; [B C D E F G H]; I{0.05h}]),
                         size = (1100, 750));
            


# Plots densities showing predicted vote
density_fed = plot_density(fed_results,
                           colours_fed,
                           "Estimated vote intention at the federal level: $day_title";
                           annotationy = -30.0);






# Save images
savefig(trend_fed, "Images/Federal/can_vote_intention_2019_post_2021.png")
savefig(house_effects_fed, "Images/Federal/can_house_effects_pollsters_2019_2022.png")
savefig(density_fed, "Images/Federal/can_vote_intention_2022.png")
savefig(trend_180_fed, "Images/Federal/can_vote_intention_last_180_days.png")




show_results(fed_results)



