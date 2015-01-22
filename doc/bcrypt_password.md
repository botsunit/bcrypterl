

# Module bcrypt_password #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-1">create/1</a></td><td>Equivalent to <a href="#create-2"><tt>create(Secret, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#create-2">create/2</a></td><td>
Hashes a secret, returning a hash.</td></tr><tr><td valign="top"><a href="#is-2">is/2</a></td><td>
Compares a potential secret against the hash.</td></tr><tr><td valign="top"><a href="#split_hash-1">split_hash/1</a></td><td>
Splits the given hash into version, cost, salt, and checksum and returns them in a list of tuples.</td></tr><tr><td valign="top"><a href="#valid_hash-1">valid_hash/1</a></td><td>
Returns true if the given hash is a valid hash.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-1"></a>

### create/1 ###


<pre><code>
create(Secret::string()) -&gt; string() | {error, any()}
</code></pre>
<br />

Equivalent to [`create(Secret, [])`](#create-2).
<a name="create-2"></a>

### create/2 ###


<pre><code>
create(Secret::string(), Options::[] | [{cost, integer()}]) -&gt; string() | {error, any()}
</code></pre>
<br />


Hashes a secret, returning a hash. Takes an optional `cost` option, which is a
logarithmic variable which determines how computational expensive the hash is to
calculate (a `cost` of 4 is twice as much work as a `cost` of 3).
The higher the `cost` the harder it becomes for attackers to try to guess
passwords (even if a copy of your database is stolen), but the slower it is to check
users' passwords.
<a name="is-2"></a>

### is/2 ###


<pre><code>
is(H::string(), Password::string()) -&gt; true | false
</code></pre>
<br />


Compares a potential secret against the hash. Returns true if the secret is the original secret, false otherwise.
<a name="split_hash-1"></a>

### split_hash/1 ###


<pre><code>
split_hash(H::string()) -&gt; list()
</code></pre>
<br />


Splits the given hash into version, cost, salt, and checksum and returns them in a list of tuples.
<a name="valid_hash-1"></a>

### valid_hash/1 ###


<pre><code>
valid_hash(H::string()) -&gt; true | false
</code></pre>
<br />


Returns true if the given hash is a valid hash.
