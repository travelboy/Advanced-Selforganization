%%Simulation- our main program which is actually the implementation of the
%%bubble model.

%defining the number of months for the simulation
months=120;

%final_flats matrix contains 
final_flats=[];

%mortgage matrix will hold the agents who have taken the risky mortgage
%loan during the months
mortgage=[];

%matrix that keeps track of all the agents who didn't bought a flat in a
%auction; format: (agentID month)
rejected_agents=[];

%the number of agents in the model
number_of_agents=1000;

%a counter that keep track of the number of flats generated till some point
%of the simulation
flat_counter=0;

%matrix that has a role of trading history of the market-gives information
%about the successful transactions:who bought which flat for what price and
%utility and in which month; format:(agentID flatID utility_value price month) 
ownership_information=[];

%matrix containing information about the agents
agents=zeros(number_of_agents,6);

%setting the parameters of the agents
for t=1:number_of_agents

%agent salary
salary=ASpareto();
%annual interest rate
interest_r=0.05;
%monthly installment
R=0.5*salary;

q=1+interest_r/12;
%number of installments
n=360;
%value of the solvency
k=(R/q.^n).*((q.^n-1)./(q-1));
%ID of the agent
agents(t,1)=t;
%salary of the agent
agents(t,2)=salary;
%solvency of the agent
agents(t,3)=k;   
%determination of the agent
agents(t,4)=0.1;
%preferences on size of the flat 
agents(t,5)=rand(1);
%preferences on quality of the flat 
agents(t,6)=1-agents(t,5);
end

%the average salary of the agents
average_agent_salary=round(mean(agents(:,2)));

%comparation interest rate- this will be calculated each month to indicate
%should we enter speculative behavior
comp_r=0;

%tracking all the values of comp_r during the months
%format: (comparation_interest_rate month)
values_of_comp=[];

%matrix that keeps track of the average square meter price for the sold flats
%per month
average_square=[];
%matrix that keeps track of the average price for the sold flats per month
average_p=[];
%this matrix tell us the months in which the speculation mode is on
begin_of_speculation=[];

%the actual simulation
for m=1:months
  
    %flag indicating if we are entering speculation mode- mode in which
    %agents participate in the auction regardless their determination value
    spec=0;
    
    %comparing the computed rate for the last month 
    if comp_r>interest_r
    spec=1;
    begin_of_speculation=[begin_of_speculation;m];
    end

%matrix that holds the flats available for sale in the current month
available_flats=[];
%the agents who fulfill the condition to participate in month auction 
participating_agents=[];
%matrix containing the caluculated utilities for all of the participating
%agents and all of the available flats
utility=[];
%sorted utility matrix in descending order
value_utility=[];
%indexes of the sorted utilities with respect to the 'normal' utility
%matrix
index_utility=[];

%generate flats 
flats=ASgenerate_flats(flat_counter);

%increase the counter to keep track of the flat IDs
flat_counter=flat_counter+size(flats,1);

%all of the flats generated till now
final_flats=[final_flats; flats];

%determining the available flats on the market
for i=1:size(final_flats,1)
    %a flat is available only if it has no owner, which is exactly what is
    %checked here
    if final_flats(i,3)==0
      available_flats=[available_flats;final_flats(i,:)];
    end
end

%generating the participating agents and their utilities on the available
%flats on the market
for a=1:number_of_agents
    
    %an agent participates in a month auction if the speculation mode is on
    %has a positive solvency
    if (spec==1&&agents(a,3)>0)||agents(a,4)>=rand(1)
    
    %this matrix holds the same parameters as the agents matrix but has
    %smaller number of rows since not all agents participate in an
    %auction (usually)
    participating_agents=[participating_agents;agents(a,:)];
    
    %c is a index of the current agent in the list of participating agents
    c=size(participating_agents,1);
    
    %calculating the utilities of the participating agent on the available flats 
         for f=1:size(available_flats,1)
         utility(c,f)=sqrt(available_flats(f,1)*participating_agents(c,6)+available_flats(f,2)*participating_agents(c,5));
         end  
         
    %lowering the determination factor
    %agents(a,4)=0.01;
    
    %we sort the utility matrix of the agent and keep the sorted values of the
    %utility in the value_utility matrix and while with index_utility we
    %keep track of the coresponding indexes of that sorted values in the
    %utility matrix 
    [sorted_values,sorted_index]=sort(utility(c,:),'descend');
    value_utility=[value_utility;sorted_values];
    index_utility=[index_utility;sorted_index];
    else
       agents(a,4)=agents(a,4)+0.01;  
    end
    
    
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ACTUAL AUCTION%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%this matrix have the num(participating_agents) rows and num(available_flats) 
%columns and contains the prices that agents offer sorted by the
%value_utility matrix
price_flat=[];

%matrix that keep all the prices of the sold flat during the months
prices=[];

%the case of the first six months of the simulation
if(1<=m&&m<=6)
    %calculating the price of each flat, different for each agent
    
     for q=1:size(participating_agents,1)
        %calculating all the prices for the flats based on utility*solvency
        price_flat=[price_flat ;value_utility(q,:).*participating_agents(q,3)];
        %in order to avoid unnaturally big prices we say that a maximum
        %price offered can be 1000000
        price_flat=min(round(price_flat),1000000);
     
     end
  
    %the auction happens here
    for f=1:size(available_flats,1)
      
    %while bids are made for different flats 
    %(while there are values different than 0 in the coresponding column of price_flat matrix)
        while any(price_flat(:,f))==1 

            %we choose the maximum bid from the price_flat
            %column,regardless for which flat its offered
            max_value=max(price_flat(:,f));
            
            %since more agents can offer the same price we keep
            %their indexes
            max_index=find(max_value==price_flat(:,f));
    
        %case of more agents having the same max value
        if(size(max_index,1)>1)
        %choosing a random agent 
        idx = randi(length(max_index)); 
        %randomval holds the index of the agent who gained the flat
        randomval = max_index(idx);
        
       %id of the sold flat with respect to the sorted utility
       id_sold_flat=index_utility(randomval,f);
       %the utility of the sold flat with respect to the buying agent
       utility_sold_flat=value_utility(randomval,f);

       %getting the 'real' id of the flat
              for p=1:size(available_flats,1)
                    if utility_sold_flat==utility(randomval,p)
                        %the 'real' ID is in the 4th column of the
                        %available_flats matrix
                    final_flat_id=available_flats(p,4);
                    end
              end
       
     
       %filling the ownership_information matrix
       ownership_information=[ownership_information; participating_agents(randomval,1) final_flat_id utility_sold_flat max_value m];
       %the determination of the  agent who buys a flat is set to 0
       agents(participating_agents(randomval,1),4)=0;
       %the solvency of the buying agent is halved
       agents(participating_agents(randomval,1),3)=round(agents(participating_agents(randomval,1),3)/2);
       %filling the prices matrix
       prices=[prices;round(max_value) m];
 
       %we need to update the matrices involved in the auction process so
       %that they are aware of the new changes(transactions)
       %put 0 everywhere in the index_utility matrix where the id of the
       %sold flat is present
       index_utility(index_utility==id_sold_flat)=0;
       
       %index_matrix is a binary help matrix that will assist in the updating:
       %contains 1 in all the positions of the index_utility where the id of the sold flat is occuring
       %and 0 in all of the other positions
       index_matrix=(index_utility==0);
       %put a value of 0 in all the positions where there is a price offer for the sold flat 
       price_flat(index_matrix)=0;
       %put 0 in all the price offers from the agent who bought the flat
       price_flat(randomval,:)=0;
       %put a flag that the flat is not longer available
       final_flats(final_flat_id,3)=1;
       
       
              else
            %there is only one agent interested
         
      id_sold_flat=index_utility(max_index,f);
      utility_sold_flat=value_utility(max_index,f);
      
        for p=1:size(available_flats,1)
           if utility_sold_flat==utility(max_index,p)                  
           final_flat_id=available_flats(p,4);
           end
        end
   
       ownership_information=[ownership_information;participating_agents(max_index,1) final_flat_id utility_sold_flat max_value m];
       agents(participating_agents(max_index,1),3)=round(agents(participating_agents(max_index,1),3)/2);
       prices=[prices;round(max_value) m];
       agents(participating_agents(max_index,1),4)=0;
       index_utility(index_utility==id_sold_flat)=0;
       index_matrix=(index_utility==0);
       price_flat(index_matrix)=0;
       price_flat(max_index,:)=0;
       final_flats(final_flat_id,3)=1;
         end 
         
        end
    
    end
        
else
% all the rest of the months

%subprime mortgage case-after the 60th month
if m>60
%delete the (m-60)th month-all the time working with the last sixty months    
rejected_agents(rejected_agents(:,2)==m-60,:)=[];

    for b=1:size(participating_agents,1)
    %count tells many times does an agent appears in the rejected_agents
    count=size(rejected_agents(participating_agents(b,1)==rejected_agents(:,1)),1);
        if count>=10
            %the agent who can take this loan shouldn't have a flat
            %previously
            if ownership_information(:,1)~=participating_agents(b,1)
            %setting the parameters to represent subprime mortgage 
            agents(participating_agents(b,1),3)=0.5*agents(participating_agents(b,1),2)*(12/interest_r);
            participating_agents(b,3)=0.5*agents(participating_agents(b,1),2)*(12/interest_r);   
            %keeping track who took a mortgage
            mortgage=[mortgage;participating_agents(b,1) m];
            end
        end
        
    end
end
%'help' matrix in which we have calculated the differences between the utilities
%in the trading history and the utility of the flat an agent wants to buy
utility_differences=[];
%'help' matrix that has the prices offered for the flats corresponding to the ones
%from utility_differences
price=[];
%this is the matrix that actually contain the prices that are used for the
%auction by the agents
final_prices=[];
%p_flat matrix that is formed in the 3 for cycles below is an equivalent of
%the price_flat matrix we used for the first 6 months- so it actually gives
%the prices of the agents sorted by the utilities 
p_flat=[];

for l=1:size(participating_agents,1)
    
  for f=1:size(available_flats,1)
      
      for r=1:size(ownership_information,1)
          %we are interested in comparing the utilities of the last three months from the trading history 
          if ownership_information(r,5)==m-3 || ownership_information(r,5)==m-2 || ownership_information(r,5)==m-1
               
             utility_differences=[utility_differences; sqrt(final_flats(ownership_information(r,2),1)*participating_agents(l,6)+final_flats(ownership_information(r,2),2)*participating_agents(l,5))-value_utility(l,f)];
             %the biggest price an agent can offer can't be bigger than his solvency!       
             price=[price; min(round(ownership_information(r,4)), round(participating_agents(l,3)))];
                   
          end  
          
       end

           utility_differences=abs(utility_differences);
           %we are interested in the minimum difference between the
           %utilities or the most similar utility value; taking the index
           %of that value
           [~,inn]=min(utility_differences);
           %offer the coresponding price of the flat with the min diff
           %utility
           price_offer=round(price(inn)+price(inn)*(rand(1)/5-0.1));
          
           final_prices=[final_prices;price_offer];
          
           utility_differences=[];
           price=[];
         
  end
  
   final_prices=final_prices';
  
   p_flat=[p_flat; final_prices];
   final_prices=[];
   
end  

%after we get the p_flat matrix the auction process is pretty similar

for f=1:size(available_flats,1)
       
        while any(p_flat(:,f))==1 
            
            max_value=max(p_flat(:,f));
           
            max_index=find(max_value==p_flat(:,f));
    
        
         if(size(max_index,1)>1)
        
            idx = randi(length(max_index)); 
      
            randomval = max_index(idx);
       
            id_sold_flat=index_utility(randomval,f);
            utility_sold_flat=value_utility(randomval,f);
       
             for p=1:num_flats
                if utility_sold_flat==utility(randomval,p)
                 final_flat_id=available_flats(p,4);
                end
             end
       ownership_information=[ownership_information; participating_agents(randomval,1) final_flat_id utility_sold_flat max_value m];
       agents(participating_agents(randomval,1),3)=round(agents(participating_agents(randomval,1),3)/2);
       prices=[prices;round(max_value) m];
       agents(participating_agents(randomval,1),4)=0;
       index_utility(index_utility==id_sold_flat)=0;
       index_matrix=(index_utility==0);
       p_flat(index_matrix)=0;
       p_flat(max_index,:)=0;
       final_flats(final_flat_id,3)=1;
       %only one agent interested
              else
            
       id_sold_flat=index_utility(max_index,f);
       utility_sold_flat=value_utility(max_index,f);
        
       for p=1:size(available_flats,1)
           if utility_sold_flat==utility(max_index,p)  
           final_flat_id=available_flats(p,4);
           end
        end
       
     
      
       ownership_information=[ownership_information; participating_agents(max_index,1) final_flat_id utility_sold_flat max_value m];
       prices=[prices;round(max_value) m];
       agents(participating_agents(max_index,1),3)=round(agents(participating_agents(max_index,1),3)/2);
       agents(participating_agents(max_index,1),4)=0;      
       index_utility(index_utility==id_sold_flat)=0;
       index_matrix=(index_utility==0);
       p_flat(index_matrix)=0;
       p_flat(max_index,:)=0;
       final_flats(final_flat_id,3)=1;
         end 
         
        end 
end
    

   
end
 %    see how many agents failed to gain apartment
     month_matrix(1:size(participating_agents,1))=m;
     rejected_agents=[rejected_agents;participating_agents(:,1) month_matrix'];
     month_matrix=[];
     
    for d=1:size(ownership_information,1)
        if ownership_information(d,5)==m
     rejected_agents(rejected_agents(:,1)==ownership_information(d,1),:)=[];
           
        end
    end 
%calculating the mean of the prices of the sold flats for a particular
%month m
average_p=[average_p;mean(prices(prices(:,2)==m)) m];

%check the interest rate of return on properties
if m>12
    %comparing the avergage prices of sold flats between curent month and 12
    %months ago
    comp_r=average_p(average_p(:,2)==m)/average_p(average_p(:,2)==m-12)-1;
    values_of_comp=[values_of_comp; comp_r m];
else
    comp_r=0;
    values_of_comp=[values_of_comp; comp_r m];
end

%end of the simulation for months
end

%we want to remove the repeating agent IDs of the subprime mortgage 
[junk,inde] = unique(mortgage(:,1),'first');       
mortgage=sort(mortgage(sort(inde)));  
