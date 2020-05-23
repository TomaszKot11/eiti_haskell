n = 3
nums = [int(x) for x in input().split()]
print(all([(len(set([nums[i*n+x]for x in range(n)])) == n)*(len(set([nums[i+n*x]for x in range(n)])) == n)for i in range(n)]))