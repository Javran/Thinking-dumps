function add(acc,current,ind)
{
    return acc + current;
}

console.log(process.argv.slice(2).map(Number).reduce(add,0));
