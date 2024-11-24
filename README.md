# Optimizing Electric Vehicle Battery Pack Performance for Endurance Racing

The Cornell FSAE team aims to design an electric vehicle that must adhere to a design ruleset to finish a 22 km endurance race in the least amount of time possible, however, they need help optimizing the vehicle for a fast finishing time while still being able to finish the race reliably.

For the 22 km electric vehicle endurance race, the primary goal is to complete the race as quickly as possible. However, sometimes, Cornell FSAE’s electric vehicle is unable to complete its 22 km endurance race. This is because the electric battery pack dies. Currently, there is not much systems analysis done on why or how to predict this failure. If it's not possible to finish the race, the next priority is to cover as much distance as possible. Finishing the race is always preferred over not finishing, even if it means a slower time. Ideally, the car is left with 5% charge left in the battery at the end of the race.

Right now, the legendary Cornell FSAE project team has both a problem and an opportunity; its slogan is “Design, Build, Win,” but since the team transitioned from gas power engine to an electric battery pack powered car, the team has not been winning—at least reliably and consistently. A key part of the team’s competition is finishing the 22 km endurance race. With the transition to an electric vehicle, the team’s car has been struggling to reliably finish the 22 km race, while also going fast. A systems analysis of FSAE’s vehicle design, focusing on its powertrain, provides a great opportunity to improve this outcome, and launch CU FSAE to victor status.

We will create a vehicle performance model that will allow for optimal system design for the vehicle to minimize total race time while ensuring high reliability in finishing the race, thus maximizing potential earned points for the team.

Multivariate linear regression:
Can we predict what factors influence the difference of the car battery pack’s SOC % versus the optimal SOC % for that time? This is done in the hopes of minimizing the difference between the optimal SOC discharge curve and the racecar’s SOC discharge curve. This will increase our likelihood of finishing the race.

Response Surface Methodology - Design Optimization
What is the optimal combination of factors to ensure that our battery depletion during a 22km race is as close to optimal as possible (depleting as much charge as possible to go as fast as possible but ending the race with 5% charge)
We will use a linear regression to determine which factors are significant predictors for minimizing the difference between the optimal battery depletion and our own data’s observed battery depletion. 
We will make an RSM to help Cornell FSAE optimize their race performance by modeling the relationship between factors influencing optimal battery depletion and recommending the optimal combination of factors to maximize the chance of optimal battery depletion
