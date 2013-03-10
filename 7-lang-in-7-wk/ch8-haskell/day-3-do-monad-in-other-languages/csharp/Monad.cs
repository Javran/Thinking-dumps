using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Monad
{
	public class IdentityMonad<T>
	{
		public static IdentityMonad<T> Return(T value)
		{
			return new IdentityMonad<T>(value);
		}

		public T Value { get; private set; }

		public IdentityMonad(T value) { this.Value = value; }

		public IdentityMonad<TTo> BindM<TTo>(Func<T, IdentityMonad<TTo>> f)
		{
			var mNew = f(Value) as IdentityMonad<TTo>;
			return IdentityMonad<TTo>.Return( mNew.Value );
		}

	}

	public class MaybeMonad<T>
	{
		public readonly bool isNothing;
		public readonly T value;

		// this might not be a common practice of C#
		// only a quick and dirty way of expressing Nothing
		public static MaybeMonad<T> Nothing() { return new MaybeMonad<T>(); }
		private MaybeMonad() {
			isNothing = true;
			value = default(T);
		}

		public static MaybeMonad<T> Return(T value)
		{
			return new MaybeMonad<T>(value);
		}

		public MaybeMonad(T val)
		{
			isNothing = false;
			value = val;
		}

		public MaybeMonad<TTo> BindM<TTo>(Func<T, MaybeMonad<TTo>> f)
		{
			if (isNothing)
			{
				return MaybeMonad<TTo>.Nothing();
			}

			var newM = f(value);
			return newM;
		}

		public override bool Equals(object obj)
		{
			var that = obj as MaybeMonad<T>;
			if (that == null)
				return false;
			return (this.isNothing == that.isNothing) ||
				(this.value.Equals(that.value));
		}

		public override int GetHashCode()
		{
			return isNothing ? 0 : value.GetHashCode();
		}
	}

	// a container that has capacity limit
	public class Container
	{
		public readonly int capacity;
		public MaybeMonad<int> currentCapa;

		public Container(int capacity)
		{
			this.capacity = capacity;
			currentCapa = new MaybeMonad<int>(0);
		}

		public void change(int diff)
		{
			// if current capacity is out of range
			// the container will be broken and not working

			var capaM = currentCapa.BindM( capa => {
				if (capa + diff<0 || capa + diff > capacity)
					return MaybeMonad<int>.Nothing();
				else
					return MaybeMonad<int>.Return( capa + diff );
			});

			currentCapa = capaM;
		}

		public override string ToString()
		{
			if (currentCapa.isNothing)
			{
				return "Container: error";
			} else {
				return string.Format("Container: {0}/{1}", currentCapa.value, capacity);
			}
		}
	}

	class Program
	{
		static void Main(string[] args)
		{
			System.Console.WriteLine( "Task #4: implement monad in a nonfunctional language" );

			// perform 1 * 2 + 30
			var monadId = IdentityMonad<int>.Return(1).BindM( x =>
				IdentityMonad<int>.Return( x * 2 ).BindM( y =>
					IdentityMonad<int>.Return( y + 30 ))
			);

			System.Console.WriteLine( "IdentityMonad test result: {0}", monadId.Value);
			
			System.Console.WriteLine( "MaybeMonad test:" );
			// the container will be broken because: 10 - 8 - 3 < 0
			//     despite 10 - 8 - 3 + 3 > 0
			var container = new Container(10);
			new int [] {10, -8, -3, 3}.Aggregate(container,
				(c, x) => {
					c.change(x);
					System.Console.WriteLine( c);
					return c;
				});

			// nothing wrong in the following example
			container = new Container(10);
			new int [] {10, -8, 3, -3}.Aggregate(container,
				(c, x) => {
					c.change(x);
					System.Console.WriteLine( c);
					return c;
				});
			
		}
	}
}
