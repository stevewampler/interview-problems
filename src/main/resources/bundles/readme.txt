Product Bundling

Background:
One of the problems ??? deals with is how to group (or bundle) products together to get the best
possible price for our customers.  For instance, if a customer orders products X, Y, and Z, we may find the best
possible price by ordering products X and Z from retailer A and product Y from retailer B.

Goal:
Given a set of products (think: items in your shopping cart) and a set of possible bundles, find the cheapest set of
bundles that covers all products within an order.

Requirements:
Write a solution that takes in the products from products.json and considers all bundles in bundles.json and returns an
array of bundles where the set contains exactly all products once and has the lowest possible price to complete the
order.

Files:
[dataset]/products.json
[dataset]/bundles.json

Output:
{
	"bundle_ids": [], // ex [1,2,3]
	"order_total": 0 // ex 123.45
}

We've provided 4 sample data sets for you to test your code against: [small, medium, original, large].  Each data set
is in its own directory, e.g. "small/" and contains a products.json file, a bundles.json file, and a solution.json file.
We've also provided a simple test harness so you can check your output against the solution.  You can run the test
harness as follows:

$ [your program and its arguments] | python harness.py [dataset directory]

e.g., if your code is called bundle.py and takes as a parameter the data set directory:

$ python bundle.py small/ | python harness.py small/