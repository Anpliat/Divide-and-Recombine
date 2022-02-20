# Divide and Recombine (D&R)

### Scenario
The data relates to telemarketing phone calls to sell long-term deposits. Within a campaign, the agents make phone calls to a list of clients to sell the product (outbound) or, if meanwhile the client calls the contact-center for any other reason, he is asked to subscribe the product (inbound). Thus, the contact is either unsuccessful or successful.
This study considers real data collected from one of the retail bank, from May 2008 to June 2010, in a total of 39883 phone contacts. More than one contact with the same client was frequently required in order to determine whether the product (bank term deposit) would be subscribed ('yes') or not ('no').

Consider a logistic regression model for the SUBSCRIBED variable using as explanatory variables:
* job
* marital
* education
* previous
* month
* cons.price.idx
* cons.conf.idx
* euribor3m
* nr.employed

### Task
Run the model using all the data. Then, use the divide and recombine approach with 10 and 20 splits. The project's goal is to compare the different estimation approaches and see how much they agree.
