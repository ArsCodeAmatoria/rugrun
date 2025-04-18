import React from 'react'
import UserProfile from '@/components/UserProfile'

export default function ProfilePage({ params }: { params: { address: string } }) {
  const { address } = params
  
  return (
    <div className="container mx-auto py-12">
      <h1 className="text-3xl font-bold text-center text-white mb-8">User Profile</h1>
      <UserProfile address={address} />
    </div>
  )
} 