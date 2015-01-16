%%ASgenerate_flats
%Function that generate flats each month in our simulaton.
%input: flat_counter - integer number indicating the number of flats
%generated till the current month. Needed so that we have the apropriate
%IDs of the flats
%output: flats- matrix containing the new generated flats
function flats=ASgenerate_flats(flat_counter)

%number of flats each month
num_flats=poissrnd(1000/120);
%empty zero matrix
flats=zeros(num_flats,4);

%determining the features of the flat
for n=1:num_flats
%quality of flat
flats(n,1)=rand(1);
%size of flat
flats(n,2)=rand(1)+0.35;
%owner- flag variable (initialy set to 0 as these flats have no owner)
flats(n,3)=0;
%keep index of the flat
flats(n,4)=n+flat_counter;

end

end