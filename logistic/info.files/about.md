# About
Jacob Townson & Andrew Giles  
July 21, 2015  

# Overview
This app works with the math behind an evolution and ecology course. It is a joint project between Jacob Townson and Andrew Giles of Georgetown College for a research project. The app is used to show a population growth rate when there is a specific carrying capacity, birth rate, and death rate. Currently, the app is not finished for its actual purpose, which is to help teach the evolution and ecology class, however, the main graph used to explain the growth is given under the *Population Size* tab. Below we will explain the purpose and uses of the rest of the tabs, as well as the meaning behind the variables input by the user in the sidebar. To see our code, and more details on how we did our work, feel free to visit our github repository for this project at <a href = "https://github.com/agiles231/shinyBio" target = "_blank"> this link. </a>

# Model Description

## Variables and their Relations

Our group made this model hoping to make it in such a way that we could relate the simulated graph to the theoretical one in an abstract way. So to start, let us establish the following notation:

- $n(t) =$ population at time $t$

- $n_0 =$ initial population

- $b =$ unconstrained birth rate (or the maximum birth rate)

- $d =$ unconstrained death rate (or the minimum death rate)

- $m =$ carrying capacity (max population that an environment can safely sustain)

- $L_t =$ number of litters at time $t$

- $S_t^i =$ size of the $i^{th}$ litter born at time $t$. These are independent of each other and of $L_t$

- $B_t =$ number of births at time $t$

Now that these variables have been established, we can attempt to relate them together. To start, let's make two more variables just to help the overall calculations: let $$s_t = E(S_t^i)$$ and $$l_t = \frac{E(L_t)}{n}$$ 

We know that the number of births at time $t$ will be the sum of the sizes of all of the litters, so $B_t = \sum_{i=1}^{L_t} S_t^i$. We can call the expected birth rate $b_t = \frac{E(B_t)}{n}$. We can assume then that the expected birth rate is the expected litter rate times the expected size of the litters, or $E(B_t) = E(S_t^i)\times E(L_t)$. This can be proven as well using what we know about each variable. When this simplified becomes $$b_t = s_t\times l_t$$ The $s_t$ and the $l_t$ are what we simulate using the rpois() function in R. After some research, we found that the average litter sizes for rabbits was $8$, so we let the $$s_t = \frac{8\sqrt{b_t}}{\sqrt{b}}$$ and $$l_t = \frac{\sqrt{b_t}}{8}\times \sqrt{b}$$ in order to be able to calculate the the number of births at time $t$ for the simulated numbers. Then to calculate the number of deaths for each time interval for the simulation, we made it a function of how close the population was to the carrying capacity so that $$d_t = d + max(0, b(\frac{n}{m}-1))$$ When adding these two things together, we got our simulated data.

## The Theoretical Differential Equation

We know that the rate of population growth is going to be the the number of rabbits times the birth rate at that specific time minus the death rate at that specific time. To put this in terms of math, we would write it as $\frac{dn}{dt} = nb_t-d_t$ which simplifies to be $$\frac{dn}{dt} = n(\frac{b-d}{m})(m-n)$$ by our model. To solve this theoretical equation, we must use knowledge of differential equations. To start, we want to move all of the $n$ values to one side of the equation to make it easy to integrate. This leads us to get $$\int \frac{dn}{n(m-n)} = \int \frac{b-d}{m} dt = \frac{b-d}{m} t + C$$ where C is some constant that we will find later. So now we want to integrate the left side of the equation. In order to do this, we will need to use partial fractions to simplify it to make it easy to integrate. After doing this, we get that $\int(\frac{1}{mn}+\frac{1}{m^2-mn})dn = \frac{b-d}{m} t + C$. When we integrate this simpler integral and apply some natural log rules, we get that $$\frac{1}{m}(ln(\frac{n}{m-n})) = \frac{b-d}{m} t + C$$ 

Now we want to find what the $C$ value is. In order to do this, let $t=0$ so $n=n_0$. After simplifying this equation for $C$, we get that $C = \frac{1}{m}(ln(\frac{n_0}{m-n_0}))$. This gives us that $$\frac{1}{m} ln(\frac{n}{m-n}) = \frac{b-d}{m}t + \frac{1}{m}ln(\frac{n_0}{m-n_0})$$ 

Finally we need to make n the outcome of the equation. This mostly just involved some elementary algebra, which lead us to get that $$n = \frac{m}{1+(\frac{m-n_0}{n_0})e^{(d-b)t}}$$ if $m > n_0$. This was our equation for the theoretical model. However, we realized later that you cannot take the natural log of a negative number, so to account for this, we put everything in the natural logs to be the absolute value. To account for this, we found that $$n= \frac{-m}{\frac{n_0-m}{n_0}e^{(d-b)t}-1}$$ if $n_0 > m$. This is our model for the theoretical graph. 

After some research, our group found that this model follows the theoretical equation for biologists as well. Their equation was $$N(t) = \frac{K}{1+(\frac{K-N(0)}{N(0)})e^{-rt}}$$ found by Roughgarden(1979), Emlen(1984), and Neuhauser(2000). This relates to our equation because $N(t) = n$, $K=m$, $N(0)=n_0$, and $r = b-d$.

# Sidebar

- **Choose Plot**: This chooses whether you want to display the theoretical graph, the simulated data, or both under the *Population Size* tab.

- **Extent of Time**: This lets you choose the time value displayed on the x axis of the graph under the *Population Size* tab.

- **Initial Population**: This lets you choose the initial population, or the y intercept under the *Population Size* tab.

- **Max Birth Rate**: This lets you choose the maximum birth rate for the population. The effective birth rate will change according to how close the population gets to the carrying capacity.

- **Min Death Rate**: This lets you choose the minimum death rate for the population. The effective death rate will change according to how close the population gets to the carrying capacity.

- **Carrying Capacity**: This lets you choose the carrying capacity for a population. It could be thought of as the amount of resources a population has to survive/thrive. Note, carrying capacity option is removed if death rate is above birth rate because a carrying capacity doesn't affect this given situation.

- **Setting the Seed**: This check box gives the user the option to set the seed of the simulated values. By setting the seed, the user can get the same outcomes for every time the *Simulate* button is pushed while the seed is set to that specific value. When the box is unchecked, the option goes away and the simulation is random again.

- **Simulate**: This button runs the app with a new simulation with the given input parameters.

# Population Size

This section is the main portion of the application. This tab displays the chosen graphs from the sidebar and the carrying capacity line (if applicable) on a single graph. This graph can be used to explain to students how the population grows theoretically, and the simulated line shows how the situation may actually pan out in reality. If the maximum birth rate is higher than the minimum death rate, both the chosen graphs and the carrying capacity will be displayed. If the minimum death rate is higher than the maximum birth rate, the carrying capacity will not be displayed. In order to run the simulation, and print the new graph with the given inputs from the sidebar, the user must press the simulate button at the bottom of the sidebar. Some more information is given below the graph in the tab.

# Growth Rate

This portion of the app is not currently finished. More details will come on it later.

# Field

This portion of the application displays a model for the field, a hypothetical for the simulated data shown in the *Population Size* tab. The idea is that this graph presents a field that rabbits (the population) live in, and if you hit the play button or choose a time on the slider within the tab, you can watch the population grow and shrink within the range of the input of the *Extent of Time* variable. 

# Graveyard

This portion of the application is similar to the field in that it is a model used to present hypothetical information. It works in parallel to the field model, but instead of growing and shrinking, it just grows with the death from the simulation. So for every "rabbit" that dies in the population, a gravestone is added to the graveyard.

<center> <h1>Thanks For Using Our App!</h1> </center>
