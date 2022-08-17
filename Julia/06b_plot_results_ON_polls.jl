colours_ON = Dict("Liberal" => :red,
                   "PC" => :blue,
                   "NDP" => :orange,
                   "Green" => :green,
                   "Other" => :yellow)

# plot trend from 2014 election
trend_ON = plot_trend(ON_results,
                      polls_ON,
                      colours_ON,
                      "Estimated vote intention of Ontario voters:\n2018 to 2022";
                      yticks = ([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, .6, .7], 
                                ["0", "10", "20", "30", "40", "50", "60", "70"]),
                      ylims = (0, .7));

# plot trend for last 180 days
trend_180_ON = plot_trend(ON_results,
                          polls_ON,
                          colours_ON,
                          "Estimated vote intention of Ontario voters:\nLast 180 days",
                          :last180;
                          ylims = (0, .65),
                          legend = :topleft,
                          yticks = ([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
                                    ["0", "10", "20", "30", "40", "50", "60"]),
                          annotationy = -0.1);

# Plot house effects
plt_house_ON, title_ON, ann_ON = plot_house(ON_results,
                                            "House effects in Ontario: 2014 to 2022")

house_effects_ON = plot(title_ON,
                        plt_house_ON[1],
                        plt_house_ON[2],
                        plt_house_ON[3], 
                        plt_house_ON[4],
                        plt_house_ON[5],
                        ann_ON,
                        layout = @layout([A{0.01h}; [B C D E F]; G{0.05h}]);                            size = (1100, 750));



# Plots densities showing predicted vote
density_ON = plot_density(ON_results,
                          colours_ON,
                          "Estimated vote intention of Ontario voters:\n$day_title";
                          annotationx = 0.65,
                          annotationy = -12.,
                          xticks = ([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6], 
                                  ["0", "10", "20", "30", "40", "50", "60"]));







# Save images
savefig(trend_ON, "Images/Ontario/ON_vote_intention_2014_2022.png")
savefig(house_effects_ON, "Images/Ontario/ON_house_effects_pollsters_2014_2022.png")
savefig(density_ON, "Images/Ontario/ON_vote_intention_2022.png")
savefig(trend_180_ON, "Images/Ontario/ON_vote_intention_last_180_days.png")


show_results(ON_results)
