function part_of_the_day() {
    let hr = (new Date()).getHours();
    if (hr >= 0 && hr < 6)
	return "night";
    if (hr >= 6 && hr < 12)
	return "morning";
    if (hr >= 12 && hr < 18)
	return "afternoon";
    if (hr >= 18 && hr < 0)
	return "evening";
}

print(`\nGood ${part_of_the_day()}, ${process.env.USER}!`);
print('Welcome to the Mongodb!\n')

EDITOR="/usr/bin/vim";
