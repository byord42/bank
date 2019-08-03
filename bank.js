/* --------------------------------- TYPES ---------------------------------- */
/*
 * Account
 *   Models a bank account.
 *
 * Fields:
 *   balance: number
 *     Current amount of dollars in account.
 * Methods:
 *   deposit: (amount: number) => Account
 *     Returns Account with balance increased by ${amount}.
 *   withdraw: (amount: number) => Account
 *     Returns Account with balance decreased by ${amount}.
 *   log: () => undefined
 *     Side effect: Logs the current balance.
 *
 * Factories: 
 *   Account: () => Account
 *     Creates Account with balance 0.
 *   Account: (initialAmount: number) => Account
 *     Creates Account with balance ${initialAmount}.
 */
const Account = (() => {
    const makeAccount = (slips) => {

        // Fields
        const balance = slips.reduce((sum, value) => sum + value);

        // Methods
        const deposit = (amount) => makeAccount([...slips, amount]);
        const withdraw = (amount) => makeAccount([...slips, -amount]);
        const log = () => console.log(balance);

        return Object.freeze({
            // Fields
            balance,

            // Methods
            deposit,
            withdraw,
            log,
        });
    };

    // Factory
    return (initialAmount = 0) => makeAccount([initialAmount]);
})();

/*
 * Customer
 *   Models a customer who may buy kittens.
 *
 * Fields:
 *   balance: number
 *     Current funds in dollars.
 * Methods:
 *   buyKitten: (price: number) => Customer
 *     Returns a Customer with one more kitten and ${price} less funds.
 *   log: () => undefined
 *     Side effect: Logs the current funds and number of kittens.
 *
 * Factories: 
 *   Customer: (funds: number) => Customer
 *     Creates Customer with no kittens and $${funds}.
 */
const Customer = (() => {
    const makeCustomer = (account, kittenCount) => {

        // Fields
        const {
            balance
        } = account;

        // Methods
        const buyKitten = (price) =>
            makeCustomer(account.withdraw(price), kittenCount + 1);

        const depositValue = (money) => {
            return {
                customer: makeCustomer(account.deposit(money))
            }
        };
        const log = () =>
            console.log(`You have $${balance} and ${kittenCount} kittens!`);

        return Object.freeze({
            // Fields
            balance,

            // Methods
            buyKitten,
            depositValue,
            log,
        });
    };

    // Factories
    return (funds) => makeCustomer(Account(funds), 0);
})();

/*
 * Shop
 *   Models a shop which sells kittens.
 *
 * Fields:
 *   price: number
 *     Current price for one kitten.
 * Methods:
 *   sellKittenTo: (customer: Customer) => {customer: Customer, shop: Shop}
 *     Returns Customer and Shop representing the state after selling one
 *       additional kitten to customer.
 *   log: () => undefined
 *     Side effect: Logs the current price for kittens.
 *
 * Factories: 
 *   Shop: (funds: number, price: number) => Shop
 *     Creates Shop with $ ${funds} selling kittens at $${price}.
 */
const Shop = (() => {
    const makeShop = (account, price) => {

        // Methods
        const sellKittenTo = (customer) => {
            return {
                customer: customer.buyKitten(price),
                shop: makeShop(account.deposit(price), price + 2),
            };
        };
        const log = () =>
            console.log(`The shop is selling a kitten for $${price}.`);

        return Object.freeze({
            // Fields
            price,

            // Methods
            sellKittenTo,
            log,
        });
    };

    return (funds, price) => makeShop(Account(funds), price);
})();

/*
 * Iterable<R>, where typeof R !== 'function'
 *   Models the iteration of a loop.
 *
 * Fields:
 *   isFinished: bool
 *     Indicates whether this iteration is the final one.
 *   returnValue: R
 *     The value computed by the iteration (undefined if not finished).
 * Methods:
 *   next: () => Iterable<R>
 *     Function to perform iteration and return next Iterable.
 *
 * Factories: 
 *   Iterable: (nextOrReturnValue: () => Iterable<R>) => Iterable<R>
 *     Creates an iterable which is not finished.
 *   Iterable: (nextOrReturnValue: R) => Iterable<R>
 *     Creates an iterable which is finished.
 */
const Iterable = (nextOrReturnValue) => {

    // Fields
    const isFinished = typeof nextOrReturnValue !== 'function';
    const returnValue = isFinished ? nextOrReturnValue : undefined;

    // Methods
    const next = isFinished ? undefined : nextOrReturnValue;

    return Object.freeze({
        // Fields
        isFinished,
        returnValue,

        // Methods
        next,
    });
};

/* ------------------------------ FUNCTIONS --------------------------------- */

/* 
 * iterateOnClick: (iterable: Iterable<R>) => undefined
 *   Side effect: Sets the onclick response of the 'buy' button to iterate
 *     through the Iterable and set its label to the return value of
 *     the iteration once it has finished.
 */
const iterateOnClick = (iterable) => {
    const element = document.getElementById('buy');
    element.onclick = () => {
        const nextIterable = iterable.next();
        if (nextIterable.isFinished) {
            element.onclick = undefined;
            element.innerHTML = nextIterable.returnValue;
        } else {
            iterateOnClick(nextIterable);
        }
    };
};

const iterateOnClick2 = (iterable) => {
    const element = document.getElementById('deposit');
    element.onclick = () => {
        const nextIterable = iterable.next();
        if (nextIterable.isFinished) {
            element.onclick = undefined;
            element.innerHTML = nextIterable.returnValue;
        } else {
            iterateOnClick(nextIterable);
        }
    };
};

/* 
 * makePurchaseIteration: ({customer: Customer, shop: Shop}) => Iterable<string>
 *   Creates an Iterable which buys kittens until the money runs out.
 */
const makePurchaseIteration = ({
    customer,
    shop
}) => {
    if (customer.balance < shop.price) {
        return Iterable(`You cannot afford more kittens :(`);
    }
    return Iterable(() => {
        const state = shop.sellKittenTo(customer);
        state.customer.log();
        state.shop.log();
        return makePurchaseIteration(state);
    });
};

const depositValue = ({
    customer
}) => {
    return Iterable(() => {
        const state = customer.depositValue(20);
        state.customer.log();
        return depositValue(state);
    });
}

/* --------------------------------- MAIN ----------------------------------- */

const main = () => {

    const customer = Customer(305);
    const shop = Shop(6000, 10);
    customer.log();
    shop.log();

    iterateOnClick(makePurchaseIteration({
        customer,
        shop
    }));
    iterateOnClick2(depositValue({
        customer
    }));
};

main();
