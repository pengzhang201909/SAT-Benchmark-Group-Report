# SAT-Benchmark-Group-Report
By Micheal Chen , Mingyang Su and Alex P Zhang

Our goal is to practice and develop our Exploratory Data Analysis(EDA) skills in R. 

In this project we analyze the distributions of SAT Benchmark Performance among high schools in the state of Connecticut from 2012 to 2013, then try to find the relationship between the number of senior students and their SAT Benchmark Performance.

This project uses a primary dataset which(SAT_School_Participation_and_Performance__2012-2013.csv) has been downloaded from the link:
https://catalog.data.gov/dataset/sat-school-participation-and-performance-2012-2013.


The SAT benchmarks are designed to measure the college readiness of high school students, using the SAT, a college entrance examination taken by nearly 1.45 million students in all 50 United States and the District of Columbia. The SAT benchmark determined in this study was 1550 for the composite. According to research conducted by the College Board, a score of 1550 indicates that a student will have a 65 percent or greater likelihood of achieving a B- average or higher during the first year of college.
(College Board. 250 Vesey Street, New York, NY 10281. Tel: 212-713-8000; e-mail: research@collegeboard.org; Web site: http://research.collegeboard.org)

The primary dataset provided SAT Benchmark Meeting and participation rate, but it did not exactly show how many senior students reach the Benchmark, and the Percent among the total number of senior students in the schools. Therefore, we created a  new index called BMR(Benchmark Meeting Rate),which comes through the number of Benchmark-Meeting seniors divided by the number of total seniors in the same school. We use BMR to evaluate SAT Benchmark Performance among high schools in Connecticut in 2012 and 2013. 

BMR is calculated as such:

bmr = number of meeting Benchmark / number of total seniors 

= (t_takes*perc_mb) / (t_takes/part_rate) 

= pec_mb*part_rate
