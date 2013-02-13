package de.tud.cs.st.bat.resolved.analyses.intraprocedural.structure

import de.tud.cs.st.bat.resolved._


/**
 * Created with IntelliJ IDEA.
 * User: Mirko
 * Date: 13.11.12
 * Time: 13:38
 * To change this template use File | Settings | File Templates.
 */
case class Stacks(maxSize: Int, collection: List[Stack])
{

    def dup(amount: Int, offset: Int): Stacks = {
        Stacks (maxSize, collection.map (s => s.dup (amount, offset)))
    }

    def pop(amount: Int): Stacks = {
        Stacks (maxSize, collection.map (s => s.pop (amount)).distinct)
    }

    def pop(): Stacks = {
        Stacks (maxSize, collection.map (s => s.pop ()).distinct)
    }

    def swap(): Stacks = {
        Stacks (maxSize, collection.map (s => s.swap ()))
    }

    def push(t: Item): Stacks = {
        Stacks (maxSize, collection.map (s => s.push (t)))
    }

    def push(t: Type, pc: Int): Stacks = {
        Stacks (maxSize, collection.map (s => s.push (t, pc)))
    }

    def push(ts: List[Item]): Stacks = {
        if (ts == Nil)
            this
        else
        {
            push (Item.combine (ts))
        }
    }


    def combineWith(other: Stacks): Stacks = {
        if (other.maxSize != maxSize) {
            throw new IllegalArgumentException ("The attribute maxSize needs to be the same.")
        }

        Stacks (maxSize, (collection ++ other.collection).distinct)
    }

    def addStack(): Stacks = {
        Stacks (maxSize, Stack (maxSize, Nil) :: collection)
    }

    def head: List[Item] = {
        var res: List[Item] = Nil

        for (stack <- collection) {
            if (stack.size == 0)
                return Nil

            res = stack (0) :: Nil
        }

        res
    }
}
