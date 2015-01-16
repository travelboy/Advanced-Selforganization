%%ASpareto
%Function that calculates the salary of each agent based on the formula mentioned in the model 
function salary=ASpareto()

%minimum salary
min_sal=1000;
%pareto index
alpha=1.16;
a=1/alpha;
%calculated salary
salary=min_sal/(1-rand(1)).^a;

end