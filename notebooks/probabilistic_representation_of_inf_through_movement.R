# Probabilistic representation of movement data

# from DTU-DASD-ASF model (Halasa et al, 2016)
# (https://www.sciencedirect.com/science/article/pii/S0378113516302097) :


# Analysis of distances between source and receiving herds -> animals from
# certain herd types (nucleus herds) were moved further than others
# --> Two distributions model the movements from nucleus herds and from other
# herd types


#Step 1 - Number of outgoing animal movements from herd

#For each herd, sum of all movements during the year, divided by 365 (or 366) -
#then using this as the lambda in a Poisson distribution, describing number of
#daily outgoing movements
#NAM = pois(herd-specific lambda) x RelDC
#RelDC is the relative direct contact of herd i during time step t, which
#defines the probability that herd i moves animals during time step t.
#I think RelDC relates to restrictions


#Step 2 - Number of animals moved in each batch

#Uses a PERT distribution, based on size of source herd

#Herd size categories, based on Danish CHR data
#Backyard: 2-5, Small herds: 6-300, Medium size herds: 301-1200,
#Large herds: 1201-2250, Very large herds: >2250

#Number of moved animals per batch from a breeding herd for the diff herd sizes
#Backyard: PERT (min=1,mode=1,max=2),
#Small: PERT (min=1,mode=61,max=196),
#Medium: PERT (min=1,mode=85,max=415),
#Large: PERT(min=1,mode=110,max=433),
#Very large: PERT (min=1,mode=81,max=371)

#Number of moved animals per batch from other herd types for diff herd sizes
#Backyard: PERT (min=1,mode=1,max=2),
#Small: PERT (min=1,mode=25,max=290),
#Medium: PERT (min=1,mode=304,max=800),
#Large: PERT (min=1,mode=300,max=910),
#Very large: PERT (min=1,mode=290,max=800)

#If the number of animals moved exceeds the size of the receiving herd,
#then the number of animals in the batch is reduced to 1/10 or 1/35 of the size
#of the receiving herd for movements from nucleus herds and sow herds, respectively.

#Step 3 - Probability of movement being infectious

#PMI = 1 - (1 - InfAM)^n * ProbTrans
#Where PMI is prob that the movement from inf herd is an infectious movement,
#InfAM is the infectiousness of inf herd through animal movements
#n is number of moved animals within the batch
#ProbTrans is transmission probability of the virus via an animal movement -
# Not in table S4!!

#InfAM = (L+Sc+C at t-1) / (N at t-1)  for the inf herd, where N is live animals
#or in SEIR model: (E+I at t-1) / (S+E+I at t-1) for the inf herd

#Step 4 - Probability of contact between herds

#PC = DistProb * TypeProb
#DistProb is probability based on distance betw 2 herds
#TypeProb is probability based on herd types

#Step 5 - Probability of receiving herd being infected

#Based on relative direct contact (impact of movement restrictions)


